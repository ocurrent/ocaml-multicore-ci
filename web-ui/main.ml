open Lwt.Infix
open Astring

let () =
  Prometheus_unix.Logging.init ()

module Server = Cohttp_lwt_unix.Server

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let add_cors_headers (headers: Cohttp.Header.t): Cohttp.Header.t =
  Cohttp.Header.add_list headers [
    ("access-control-allow-origin", "*");
    ("access-control-allow-headers", "Accept, Content-Type");
    ("access-control-allow-methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

let normal_response x =
  x >|= fun x -> `Response x

let docroot = "web-app/build"

let handle_request ~backend ~graphql_callback conn request body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  let path = Uri.pct_decode path in
  let response_headers = Cohttp.Header.init () in
  let response_headers = add_cors_headers response_headers in
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match meth, String.cuts ~sep:"/" ~empty:false path with
  | `OPTIONS, ("graphql" :: _) ->
    Server.respond_string ~status:`OK ~headers:response_headers ~body:"" () |> normal_response
  | _, ("graphql" :: _) ->
    let resp = graphql_callback conn request body in
    let open Server.IO in
    let open Cohttp.Response in
    resp >>= (function
      | `Response(resp, body) ->
        let resp_with_cors =
            make
              ~version:resp.version
              ~status:resp.status
              ~flush:resp.flush
              ~encoding:resp.encoding
              ~headers:(add_cors_headers resp.headers) ()
        in
        return @@ `Response(resp_with_cors, body)
      | resp -> return resp
    )
  | `GET, ([] | "jobs" :: _ | "results" :: _) ->
    let index_uri = Uri.with_uri ~path:(Some "/index.html") uri in
    let fname = Server.resolve_local_file ~docroot ~uri:index_uri in
    Server.respond_file ~headers:response_headers ~fname () |> normal_response
  | `GET, (["index.html"] | ("images" :: _) | ("static" :: _)) ->
    let fname = Server.resolve_local_file ~docroot ~uri in
    Server.respond_file ~fname ~headers:response_headers () |> normal_response
  | `GET, ["css"; "style.css"] ->
    Style.get () |> normal_response
  | meth, ("github" :: path) ->
    Github.handle ~backend ~meth path
  | `GET, ("badge" :: path) ->
     Badges.handle ~backend ~path
  | _ ->
    Server.respond_not_found () |> normal_response

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

module Graphql_cohttp_lwt = Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO) (Cohttp_lwt.Body)

let main port backend_uri admin_service_uri prometheus_config =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_uri in
    let backend = Backend.make backend_sr in
    let%lwt ci = Backend.ci backend in
    let graphql_callback = Graphql_cohttp_lwt.make_callback (fun _req -> ()) (Ci_graphql.schema ~admin_service_uri ci) in
    let config = Server.make_response_action ~callback:(handle_request ~backend ~graphql_callback) () in
    let mode = `TCP (`Port port) in
    Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
    let web =
      Lwt.try_bind
        (fun () -> Server.create ~mode config)
        (fun () -> failwith "Web-server stopped!")
        (function
          | Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
            Fmt.failwith "Web-server failed.@ Another program is already using this port %a." pp_mode mode
          | ex -> Lwt.fail ex
        )
    in
    Lwt.choose (web :: Prometheus_unix.serve prometheus_config)
  end

open Cmdliner

let port =
  Arg.value @@
  Arg.opt Arg.int 8090 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]

let backend_cap =
  Arg.required @@
  Arg.opt (Arg.some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The capability file giving access to the CI backend service."
    ~docv:"CAP"
    ["backend"]

let admin_service_uri =
  Arg.value @@
  Arg.opt Arg.string "http://localhost:8080" @@
  Arg.info
    ~doc:"The public endpoint for the admin UI. This is used when linking from this frontend to the admin UI."
    ~docv:"URI"
    ["admin-service-uri"]

let cmd =
  let doc = "A web front-end for Ocaml-Multicore-CI" in
  Term.(const main $ port $ backend_cap $ admin_service_uri $ Prometheus_unix.opts),
  Term.info "ocaml-multicore-ci-web" ~doc

let () = Term.(exit @@ eval cmd)
