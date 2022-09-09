open Lwt.Infix
open Capnp_rpc_lwt

module Metrics = struct
  open Prometheus
  open Ocaml_multicore_ci

  let namespace = "ocamlci"

  let subsystem = "pipeline"

  let master =
    let help = "Number of master branches by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "master_state_total"

  type stats = {
    ok : int;
    failed : int;
    active : int;
  }

  let count_repo ~owner name (acc : stats) =
    let repo = { Current_github.Repo_id.owner; name } in
    match Index.Ref_map.find_opt "refs/heads/master" (Index.get_active_refs repo) with
    | None -> acc
    | Some hash ->
      match Index.get_status ~owner ~name ~hash with
      | `Failed -> { acc with failed = acc.failed + 1 }
      | `Passed -> { acc with ok = acc.ok + 1 }
      | `Not_started | `Pending -> { acc with active = acc.active + 1 }

  let count_owner owner (acc : stats) =
    Index.Repo_set.fold (count_repo ~owner) (Index.get_active_repos ~owner) acc

  let update () =
    let owners = Index.get_active_owners () in
    let { ok; failed; active } = Index.Owner_set.fold count_owner owners { ok = 0; failed = 0; active = 0 } in
    Gauge.set (master "ok") (float_of_int ok);
    Gauge.set (master "failed") (float_of_int failed);
    Gauge.set (master "active") (float_of_int active)
end

let solver = Ocaml_multicore_ci.Solver_pool.spawn_local ()

let () =
  Prometheus_unix.Logging.init ();
  Mirage_crypto_rng_unix.initialize ();
  Prometheus.CollectorRegistry.(register_pre_collect default) Metrics.update;
  match Conf.profile with
  | `Production -> Logs.info (fun f -> f "Using production configuration")
  | `Dev -> Logs.info (fun f -> f "Using dev configuration")

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let run_capnp = function
  | None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some public_address ->
    let config =
      Capnp_rpc_unix.Vat_config.create
        ~public_address
        ~secret_key:(`File Conf.Capnp.secret_key)
        (Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0" ~port:Conf.Capnp.internal_port)
    in
    let rpc_engine, rpc_engine_resolver = Capability.promise () in
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
    let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    Capnp_rpc_unix.Cap_file.save_service vat service_id Conf.Capnp.cap_file |> or_die;
    Logs.app (fun f -> f "Wrote capability reference to %S" Conf.Capnp.cap_file);
    Lwt.return (vat, Some rpc_engine_resolver)

(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ ->
    match Option.map Current_web.User.id user with
    | Some ( "github:talex5"
           | "github:avsm"
           | "github:ewanmellor"
           | "github:kit-ty-kate"
           | "github:samoht"
           | "github:moyodiallo"
           | "github:tmcgilchrist"
           | "github:mtelvers"
           ) -> true
    | _ -> false

let main config mode app capnp_address github_auth submission_uri : ('a, [`Msg of string]) result =
  Lwt_main.run begin
    run_capnp capnp_address >>= fun (vat, rpc_engine_resolver) ->
    let ocluster = Option.map (Capnp_rpc_unix.Vat.import_exn vat) submission_uri in
    let confs = Conf.configs in
    let engine = Current.Engine.create ~config (Pipeline.v ?ocluster ~solver ~confs) in
    rpc_engine_resolver |> Option.iter (fun r -> Capability.resolve_ok r (Api_impl.make_ci ~engine));
    let authn = Option.map Current_github.Auth.make_login_uri github_auth in
    let webhook_secret =
      match app with
      | Some app -> Current_github.App.webhook_secret app
      | None -> ""
    in
    let has_role =
      if github_auth = None then Current_web.Site.allow_all
      else has_role
    in
    let secure_cookies = github_auth <> None in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook ~engine ~webhook_secret ~has_role) ::
      Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) ::
      Current_web.routes engine in
    let site = Current_web.Site.v ?authn ~has_role ~secure_cookies ~name:"ocaml-multicore-ci" routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let capnp_address =
  Arg.value @@
  Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None @@
  Arg.info
    ~doc:"Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no RPC)"
    ~docv:"ADDR"
    ["capnp-address"]

let submission_service =
  Arg.value @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE"
    ["submission-service"]

let github_app_id =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info ["github-app-id"]

let cmd ~with_github =
  let doc = "Build OCaml projects on GitHub" in
  let term = Term.(
    let gh_cmd = if with_github then
      const (fun x -> Some x) $ Current_github.App.cmdliner
    else
      const None
    in
    term_result (
      const main $ Current.Config.cmdliner $ Current_web.cmdliner $
      gh_cmd $
      capnp_address $ Current_github.Auth.cmdliner $ submission_service)) in
  let info = Cmd.info "ocaml-multicore-ci" ~doc in
  Cmd.v info term

let () =
  let with_github = match Cmd.eval_peek_opts github_app_id with
  | (None, _) -> false
  | (Some _, _) -> true
  in
  Cmd.eval (cmd ~with_github) |> exit
