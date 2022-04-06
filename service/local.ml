(* Utility program for testing the CI pipeline on a local repository. *)

let solver = Ocaml_multicore_ci.Solver_pool.spawn_local ()

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1";
  Prometheus_unix.Logging.init ()

let make_pipeline repos =
  let repos = repos |> List.map (fun repo -> Current_git.Local.v (Fpath.v repo)) in
  match repos with
  | [] -> Pipeline.local_test_fixed ~solver
  | [repo] -> Pipeline.local_test ~solver repo
  | _ -> Pipeline.local_test_multiple ~solver repos

let main config mode repos : ('a, [`Msg of string]) result =
  let pipeline = make_pipeline repos in
  let engine = Current.Engine.create ~config pipeline in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:"ocaml-multicore-ci-local" (Current_web.routes ~docroot:"ocurrent/static" engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repos =
  let open Arg in
  value &
  pos_all dir [] &
  info
    ~doc:"The directory containing the .git subdirectory. May be specified multiple times."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Test ocaml-multicore-ci on a local Git clone" in
  let term = Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repos)) in
  let info = Cmd.info "ocaml-multicore-ci-local" ~doc in
  Cmd.v info term

let () = exit @@ Cmd.eval cmd
