open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

let src = Logs.Src.create "ocaml_multicore_ci.cluster_build" ~doc:"ocaml-ci ocluster builder"
module Log = (val Logs.src_log src : Logs.LOG)

module Image = Current_docker.Raw.Image
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

type t = {
  connection : Current_ocluster.Connection.t;
  timeout : Duration.t option;
}

module Op = struct
  type nonrec t = t

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string;                            (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t;         (* The source code to build and test *)
      compiler_commit : Current_git.Commit_id.t option;  (* The commit for the compiler build to use. If None then the one in the base image will be used. *)
      repo : string;                            (* Used to choose a build cache *)
      test_repo : string option;                       (* The repo under test, if repo is a compiler *)
      label : string;                           (* A unique ID for this build within the commit *)
      sandmark_package: string option
    }

    let to_json { pool; commit; compiler_commit; label; repo; test_repo; sandmark_package } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "compiler_commit", (match compiler_commit with None -> `Null | Some compiler_commit -> `String (Current_git.Commit_id.hash compiler_commit));
        "repo", `String repo;
        "test_repo", (match test_repo with None -> `Null | Some r -> `String r);
        "label", `String label;
        "sandmark_package",(match sandmark_package with None -> `Null | Some p -> `String p);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Image.t;                           (* The image with the OCaml compiler to use. *)
      variant : Variant.t;                      (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc [
        "base", `String (Image.digest base);
        "op", Spec.ty_to_yojson ty;
        "variant", Variant.to_yojson variant;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let get_cache_hint repo { Value.base; variant; ty } =
    Fmt.str "%s-%s-%a-%s"
      repo
      (Image.hash base)
      Variant.pp variant
      (Spec.digest_of_ty ~variant ty)

  let run t job { Key.pool; commit; compiler_commit; label = _; repo; test_repo; _ } spec =
    let { Value.base; variant; ty } = spec in
    let build_spec = Build.make_build_spec ~base ~compiler_commit ~repo ~test_repo ~variant ~ty
    in
    let build_context_commit = match compiler_commit with
    | None -> commit
    | Some cc -> cc
    in
    Current.Job.write job
      (Fmt.str "@[<v>Base: %a@,%a@]@."
         Image.pp base
         Spec.pp_summary ty);
    Current.Job.write job
      (Fmt.str "@.\
                 To reproduce locally:@.@.\
                 %a@.\
                 cat > Dockerfile <<'END-OF-DOCKERFILE'@.\
                 \o033[34m%s\o033[0m@.\
                 END-OF-DOCKERFILE@.\
                 docker build .@.@."
         Current_git.Commit_id.pp_user_clone build_context_commit
         (Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false build_spec));
    let spec_str = Fmt.to_to_string Obuilder_spec.pp build_spec in
    let action = Cluster_api.Submission.obuilder_build spec_str in
    let src = (Git.Commit_id.repo build_context_commit, [Git.Commit_id.hash build_context_commit]) in
    let cache_hint = get_cache_hint repo spec in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using OBuilder spec:@.%s@." spec_str;
    let build_pool = Current_ocluster.Connection.pool ~job ~pool ~action ~cache_hint ~src t.connection in
    Current.Job.start_with ~pool:build_pool job ?timeout:t.timeout ~level:Current.Level.Average >>= fun build_job ->
    Capability.with_ref build_job (Current_ocluster.Connection.run_job ~job) >>!= fun (_ : string) ->
    Lwt_result.return ()

  let pp f ({ Key.pool; repo; test_repo; commit; compiler_commit; label; sandmark_package }, _) =
    Fmt.pf f "test %a %s %a %a %a (%s:%s)"
      Fmt.(option string) sandmark_package
      repo (Fmt.option Fmt.string) test_repo
      Current_git.Commit_id.pp commit
      (Fmt.option Current_git.Commit_id.pp) compiler_commit
      pool label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

let config ?timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout }

let build t ~platforms ~spec ~repo ?test_repo ?compiler_commit ?sandmark_package commit =
  Current.component "cluster build@. %a" Fmt.(option string) sandmark_package |>
  let> { Spec.variant; ty; label } = spec
  and> commit = commit
  and> compiler_commit = Current.option_seq compiler_commit
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> Variant.equal p.Platform.variant variant) platforms with
  | Some { Platform.builder = _; pool; variant; base; _ } ->
    BC.run t { Op.Key.pool; commit; compiler_commit; repo; test_repo; label; sandmark_package } { Op.Value.base; ty; variant }
  | None ->
    (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
    let msg = Fmt.str "BUG: variant %a is not a supported platform" Variant.pp variant in
    Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let v t ~platforms ~repo ?test_repo ?compiler_commit ?sandmark_package ~spec source =
  let build = build t ~platforms ~spec ~repo ?test_repo ?compiler_commit ?sandmark_package source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec = spec in
  let result =
    state |> Result.map (fun () -> Spec.success_result spec)
  in
  result, job_id
