(*
  TODO
  - prometheus serving
  - parse extra cil arguments
  
 *)
let default_query =
  "{\\n  orgs {\\n    name\\n  }\\n}\\n"

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html

let schema ~admin_service_uri ci = Ci_graphql.schema ~admin_service_uri ci

let main backend_uri admin_service_uri =
  Lwt_main.run begin

    let open Lwt.Infix in
    let vat = Capnp_rpc_unix.client_only_vat () in
    let backend_sr = Capnp_rpc_unix.Vat.import_exn vat backend_uri in
    let backend = Backend.make backend_sr in
    Backend.ci backend >>= fun ci ->

    Dream.serve
    @@ Dream.logger
    @@ Dream.origin_referrer_check
    @@ Dream.router [
           Dream.any "/graphql" (Dream.graphql (fun _request -> Lwt.return ()) (schema ~admin_service_uri ci));
           Dream.get "/graphiql" (Dream.graphiql ~default_query "/graphql");
           Dream.get "/js/**" @@ Dream.static "static";
           Dream.get "/"  (fun _ -> Dream.html @@ html_to_string Template.home)
         ]
    end



(* Command-line parsing *)

open Cmdliner

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
  let doc = "A web front-end for OCaml-CI" in
  let info = Cmd.info "ocaml-ci-web" ~doc in
  Cmd.v info Term.(const main $ backend_cap $ admin_service_uri)

let () = exit @@ Cmd.eval cmd
