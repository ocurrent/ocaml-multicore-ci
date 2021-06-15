open Current.Syntax
open Lwt.Infix

module Raw = Current_docker.Raw
module Selection = Ocaml_multicore_ci_api.Worker.Selection

let checkout_pool = Current.Pool.create ~label:"git-clone" 1

(* Make sure we never build the same (commit, variant) twice at the same time, as this is likely
   to trigger BuildKit bug https://github.com/moby/buildkit/issues/1456.
   While a build is in progress, this contains a promise for the build finishing. *)
let commit_locks = Hashtbl.create 1000

let rec with_commit_lock ~job commit variant fn =
  let key = (Current_git.Commit.hash commit, variant) in
  match Hashtbl.find_opt commit_locks key with
  | Some lock ->
    Current.Job.log job "Waiting for a similar build to finish...";
    lock >>= fun () ->
    with_commit_lock ~job commit variant fn
  | None ->
    let finished, set_finished = Lwt.wait () in
    Hashtbl.add commit_locks key finished;
    Lwt.finalize fn
      (fun () ->
         Hashtbl.remove commit_locks key;
         Lwt.wakeup set_finished ();
         Lwt.return_unit
      )

let make_build_spec ~base ~repo ~compiler_commit ~variant ~ty =
    let base = Raw.Image.hash base in
    match ty with
    | `Opam (`Build, selection, opam_files) ->
         begin
           match compiler_commit with
          | None ->
            Opam_build.spec_dune ~base ~opam_files ~compiler_commit ~selection
          | Some _ ->
            Opam_build.spec_opam_install ~base ~opam_files ~compiler_commit ~selection
        end
    | `Opam (`Lint `Doc, selection, opam_files) -> Lint.doc_spec ~base ~opam_files ~selection
    | `Opam (`Lint `Opam, selection, opam_files) -> Lint.opam_lint_spec ~base ~opam_files ~selection
    | `Opam (`Make targets, selection, opam_files) -> Opam_build.spec_make ~base ~opam_files ~compiler_commit ~selection ~targets
    | `Opam (`Script cmds, selection, opam_files) -> Opam_build.spec_script ~base ~opam_files ~compiler_commit ~selection ~cmds
    | `Opam_fmt ocamlformat_source -> Lint.fmt_spec ~base ~ocamlformat_source
    | `Opam_monorepo config -> Opam_monorepo.spec ~base ~repo ~config ~variant

module Op = struct
  type t = Builder.t

  let id = "ci-build"

  let dockerignore = ".git"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t;            (* The source code to build and test *)
      compiler_commit : Current_git.Commit.t option;  (* The commit for the compiler build to use. If None then the one in the base image will be used. *)
      repo : string;                            (* Used to choose a build cache *)
      label : string;                           (* A unique ID for this build within the commit *)
    }

    let to_json { commit; compiler_commit; label; repo } =
      `Assoc [
        "commit", `String (Current_git.Commit.hash commit);
        "compiler_commit", (match compiler_commit with None -> `Null | Some compiler_commit -> `String (Current_git.Commit.marshal compiler_commit));
        "repo", `String repo;
        "label", `String label;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = {
      ty : Spec.ty;
      base : Raw.Image.t;                       (* The image with the OCaml compiler to use. *)
      variant : Variant.t;                      (* Added as a comment in the Dockerfile *)
    }

    let to_json { base; ty; variant } =
      `Assoc [
        "base", `String (Raw.Image.digest base);
        "op", Spec.ty_to_yojson ty;
        "variant", (Variant.to_yojson variant);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Outcome = Current.Unit

  let or_raise = function
    | Ok () -> ()
    | Error (`Msg m) -> raise (Failure m)

  let run { Builder.docker_context; pool; build_timeout } job
      { Key.commit; compiler_commit; label = _; repo } { Value.base; variant; ty } =
    let compiler_commit_id = Option.map Current_git.Commit.id compiler_commit in
    let build_context_commit = match compiler_commit with
    | None -> commit
    | Some cc -> cc
    in
    let build_spec = make_build_spec ~base ~compiler_commit:compiler_commit_id ~repo ~variant ~ty
    in
    let make_dockerfile ~for_user =
      (if for_user then "" else Buildkit_syntax.add (Variant.arch variant)) ^
      Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:(not for_user) build_spec
    in
    Current.Job.write job
      (Fmt.strf "@[<v>Base: %a@,%a@]@."
         Raw.Image.pp base
         Spec.pp_summary ty);
    Current.Job.write job
      (Fmt.strf "@.\
                 To reproduce locally:@.@.\
                 %a@.\
                 cat > Dockerfile <<'END-OF-DOCKERFILE'@.\
                 \o033[34m%s\o033[0m\
                 END-OF-DOCKERFILE@.\
                 docker build .@.@."
         Current_git.Commit_id.pp_user_clone (Current_git.Commit.id build_context_commit)
         (make_dockerfile ~for_user:true));
    let dockerfile = make_dockerfile ~for_user:false in
    Current.Job.start ~timeout:build_timeout ~pool job ~level:Current.Level.Average >>= fun () ->
    with_commit_lock ~job build_context_commit variant @@ fun () ->
    Current_git.with_checkout ~pool:checkout_pool ~job build_context_commit @@ fun dir ->
    Current.Job.write job (Fmt.strf "Writing BuildKit Dockerfile:@.%s@." dockerfile);
    Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
    (* Normally, we write a dockerignore so that the .git directory isn't
     * copied. However, the multicore compiler writes its git hash and
     * branch into the ocamlrun -version output, so it needs .git.
     *)
    if compiler_commit = None
    then
      Bos.OS.File.write Fpath.(dir / ".dockerignore") dockerignore |> or_raise;
    let cmd = Raw.Cmd.docker ~docker_context @@ ["build"; "--"; Fpath.to_string dir] in
    let pp_error_command f = Fmt.string f "Docker build" in
    Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd

  let pp f ({ Key.repo; commit; compiler_commit; label }, _) =
    Fmt.pf f "test %s %a %a (%s)"
      repo
      Current_git.Commit.pp commit
      (Fmt.option Current_git.Commit.pp) compiler_commit
      label

  let auto_cancel = true
  let latched = true
end

module BC = Current_cache.Generic(Op)

let build ~platforms ~spec ~repo ?compiler_commit commit =
  Current.component "build" |>
  let> { Spec.variant; ty; label } = spec
  and> commit = commit
  and> compiler_commit = Current.option_seq compiler_commit
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> Variant.equal p.Platform.variant variant) platforms with
  | Some { Platform.builder; variant; base; _ } ->
    BC.run builder { Op.Key.commit; compiler_commit; repo; label } { Op.Value.base; ty; variant }
  | None ->
    (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
    let msg = Fmt.strf "BUG: variant %a is not a supported platform" Variant.pp variant in
    Current_incr.const (Error (`Msg msg), None)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let v ~platforms ~repo ?compiler_commit ~spec source =
  let build = build ~platforms ~spec ~repo ?compiler_commit source in
  let+ state = Current.state ~hidden:true build
  and+ job_id = get_job_id build
  and+ spec = spec in
  let result =
    state |> Result.map (fun () -> Spec.success_result spec)
  in
  result, job_id
