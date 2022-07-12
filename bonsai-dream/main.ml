(*
  TODO
  - prometheus serving
  - parse extra cil arguments
  
 *)
let () =
  Prometheus_unix.Logging.init ()


let default_query =
  "{\\n  orgs {\\n    name\\n  }\\n}\\n"

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html

let schema ~admin_service_uri ci = Ci_graphql.schema ~admin_service_uri ci

let main port backend_uri admin_service_uri _docroot prometheus_config =
  Lwt_main.run begin

    let open Lwt.Infix in
    let vat = Capnp_rpc_unix.client_only_vat () in
    let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_uri in
    let backend = Backend.make backend_sr in
    Backend.ci backend >>= fun ci ->

    let web =
      Dream.serve ~port
      @@ Dream.logger
      @@ Dream.origin_referrer_check
      @@ Dream.router [
             Dream.get "/github" (fun _ -> Github.orgs ci);
             Dream.get "/github/:owner" (fun request -> 
                 let owner = Dream.param request "owner" in 
                 Github.list_repos ~owner ci);
             Dream.get "/github/:owner/:name" (fun request -> 
                 let owner = Dream.param request "owner" in
                 let name = Dream.param request "name" in
                 Github.repo ~owner ~name ci);
             Dream.any "/graphql" (Dream.graphql (fun _ -> Lwt.return ()) (schema ~admin_service_uri ci));
             Dream.get "/graphiql" (Dream.graphiql ~default_query "/graphql");
             Dream.get "/js/**" @@ Dream.static "static";
             Dream.get "/css/**" @@ Dream.static "static/css";
             Dream.get "/"  (fun _ -> Dream.html @@ html_to_string Template.home)
             (* TODO Replace with index.html loading from disk! *)
           ]
    in
    Lwt.choose (web :: Prometheus_unix.serve prometheus_config)
    end



(* Command-line parsing *)

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

let docroot =
  Arg.value @@
    Arg.opt Arg.string "_build" @@
      Arg.info
        ~doc:"The docroot for the web UI. This is used to find the built JavaScript and images etc."
        ~docv:"PATH"
        ["docroot"]

let cmd =
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const main $ port $ backend_cap $ admin_service_uri $ docroot $ Prometheus_unix.opts)

let () = exit @@ Cmd.eval cmd
