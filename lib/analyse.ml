open Lwt.Infix
open Current.Syntax
open Pipeline_utils
open Repo_url_utils
module Worker = Ocaml_multicore_ci_api.Worker

let opam_ext_re = Str.regexp "\\.opam$"
let dev_re = Str.regexp "\\.dev$"
let dune_cmd_re = Str.regexp "^dune "
let jobs_re = Str.regexp " -j jobs\\b"
let package_name_re = Str.regexp " -p name\\b"
let pool = Current.Pool.create ~label:"analyse" 2

let pp_platforms fmt platforms =
  Fmt.pf fmt "%a" Fmt.(list ~sep:(any ", ") Variant.pp) (List.map fst platforms)

let is_empty_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; st_size = 0; _ } -> true
  | _ -> false

let is_toplevel path = not (String.contains path '/')
let ( >>!= ) = Lwt_result.bind

let read_file ~max_len path =
  Lwt_io.with_file ~mode:Lwt_io.input path (fun ch ->
      Lwt_io.length ch >>= fun len ->
      let len =
        if len <= Int64.of_int max_len then Int64.to_int len
        else Fmt.failwith "File %S too big (%Ld bytes)" path len
      in
      let buf = Bytes.create len in
      Lwt_io.read_into_exactly ch buf 0 len >|= fun () -> Bytes.to_string buf)

(* A logging service that logs to [job]. *)
let job_log job =
  let module X = Ocaml_multicore_ci_api.Raw.Solve.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

let make_placeholder_selections ~platforms ~opam_repository_commits =
  platforms
  |> List.map (fun platform ->
         {
           Selection.variant = fst platform;
           packages = [];
           commits =
             List.map
               (fun y ->
                 (Current_git.Commit_id.repo y, Current_git.Commit_id.hash y))
               opam_repository_commits;
           command = None;
         })

(* Remove all the *.dev packages in opam_files from the given selections *)
let remove_dev_selections_from_opam_build ~opam_files selections =
  selections
  |> List.map (fun sel ->
         List.fold_left
           (fun acc opam_file ->
             let opam_dev = Str.global_replace opam_ext_re ".dev" opam_file in
             Selection.remove_package acc ~package:opam_dev)
           sel opam_files)

(* Remove all the *.dev packages in opam_files from the given build,
   if it uses Opam_build. This operation is not implemented for the other
   build types. *)
let remove_dev_selections ~opam_files = function
  | `Opam_build selections ->
      `Opam_build (remove_dev_selections_from_opam_build ~opam_files selections)
  | `Opam_monorepo _ as x -> x
  | `Not_opam _ as x -> x

module Analysis = struct
  type t = {
    opam_files : string list;
    ocamlformat_source : Analyse_ocamlformat.source option;
    selections :
      [ `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config
      | `Not_opam of string * Selection.t list ];
  }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  let opam_files t = t.opam_files
  let ocamlformat_source t = t.ocamlformat_source
  let selections t = t.selections
  let is_test_dir = Astring.String.is_prefix ~affix:"test"

  let dummy_analysis ~platforms ~opam_repository_commits ~package_name =
    {
      opam_files = [];
      ocamlformat_source = None;
      selections =
        `Not_opam
          ( package_name,
            make_placeholder_selections ~platforms ~opam_repository_commits );
    }

  let check_opam_version =
    let version_2 = OpamVersion.of_string "2" in
    fun name opam ->
      let opam_version = OpamFile.OPAM.opam_version opam in
      if OpamVersion.compare opam_version version_2 < 0 then
        Fmt.failwith "Package %S uses unsupported opam version %s (need >= 2)"
          name
          (OpamVersion.to_string opam_version)

  let check_all_opam_version pkgs =
    pkgs |> List.iter (fun (name, opam) -> check_opam_version name opam)

  let parse_opam_file pkg =
    let name, contents = pkg in
    try (name, OpamFile.OPAM.read_from_string contents)
    with ex -> Fmt.failwith "Invalid opam file %S: %a" name Fmt.exn ex

  let find_all_pin_depends pkgs =
    pkgs
    |> List.map (fun (name, opam) ->
           let pin_depends = OpamFile.OPAM.pin_depends opam in
           pin_depends |> List.map (fun (pkg, url) -> (name, pkg, url)))
    |> List.concat

  let log_pin_depends ~job pin_depends =
    pin_depends
    |> List.iter (fun (root_pkg, pkg, url) ->
           Current.Job.log job "%s: found pin-depends: %s -> %s" root_pkg
             (OpamPackage.to_string pkg)
             (OpamUrl.to_string url))

  let process_pin_depends ~job pin_depends =
    pin_depends
    |> Lwt_list.map_s (fun (root_pkg, pkg, url) ->
           Lwt.catch
             (fun () ->
               Pin_depends.get_opam ~job ~pkg url >|= fun contents ->
               (OpamPackage.to_string pkg, contents))
             (function
               | Failure msg ->
                   Fmt.failwith "%s (processing pin-depends in %s)" msg root_pkg
               | ex -> Lwt.fail ex))

  (* For each package in [root_pkgs], parse the opam file and check whether it uses pin-depends.
     Fetch and return all pinned opam files. Also, ensure we're using opam format version 2. *)
  let handle_opam_files ~job ~root_pkgs ~pinned_pkgs =
    let root_pkgs_parsed = root_pkgs |> List.map parse_opam_file in
    let pinned_pkgs_parsed = pinned_pkgs |> List.map parse_opam_file in
    check_all_opam_version root_pkgs_parsed;
    check_all_opam_version pinned_pkgs_parsed;
    let pin_depends = find_all_pin_depends root_pkgs_parsed in
    log_pin_depends ~job pin_depends;
    process_pin_depends ~job pin_depends

  let arg_to_string = function
    | OpamTypes.CString str, _ ->
        str (* FIXME: Is this supposed to be escaped in some way? *)
    | OpamTypes.CIdent str, _ -> str

  let args_to_string ~pkg args =
    let cmd = args |> List.map arg_to_string |> String.concat " " in
    (* If the command is a dune one, replace the -j jobs and -p name
       arguments.  These are expected in a .opam file but we need to adjust
       them for our own build. *)
    if Str.string_match dune_cmd_re cmd 0 then
      cmd
      |> Str.global_replace jobs_re ""
      |> Str.global_replace package_name_re (" -p " ^ pkg)
    else cmd

  let build_command_to_string ~pkg command =
    if command = [] then None
    else if List.for_all (fun (_, filter) -> filter = None) command then
      let strs =
        command |> List.map (fun (args, _) -> args_to_string ~pkg args)
      in
      Some (String.concat " && " strs)
    else
      (* We have not implemented processing of filters, so fall back to the
         default command. *)
      None

  (** If the opam file for the given root_pkgs includes a build command, use
      that instead of the default (dune build @install @runtests).  This is
      put into Selection.command to be picked up by the build phase.

      This is only implemented if root_pkgs is a singleton, otherwise we
      fall back to the default build command.
    *)
  let maybe_add_build_command ~root_pkgs selection =
    match root_pkgs with
    | [ (pkg_name, opam_str) ] ->
        let pkg = Str.global_replace dev_re "" pkg_name in
        let opam = OpamFile.OPAM.read_from_string opam_str in
        let command = build_command_to_string ~pkg (OpamFile.OPAM.build opam) in
        {
          Selection.variant = selection.Selection.variant;
          packages = selection.packages;
          commits = selection.commits;
          command;
        }
    | _ -> selection

  let opam_selections ~solve ~job ~platforms ~opam_files dir =
    Current.Job.log job "Solving: @[platforms=%a@,opam_files=%a@]" pp_platforms
      platforms
      Fmt.(list ~sep:(any ", ") string)
      opam_files;
    let src = Fpath.to_string dir in
    let ( / ) = Filename.concat in
    opam_files
    |> Lwt_list.fold_left_s
         (fun (root_pkgs, pinned_pkgs) path ->
           let name = Filename.basename path |> Filename.chop_extension in
           let name =
             if String.contains name '.' then name else name ^ ".dev"
           in
           read_file ~max_len:102400 (src / path) >|= fun file ->
           let item = (name, file) in
           if Filename.dirname path = "." then (item :: root_pkgs, pinned_pkgs)
           else (root_pkgs, item :: pinned_pkgs))
         ([], [])
    >>= fun (root_pkgs, pinned_pkgs) ->
    Lwt.try_bind
      (fun () -> handle_opam_files ~job ~root_pkgs ~pinned_pkgs)
      (fun pin_depends ->
        let pinned_pkgs = pin_depends @ pinned_pkgs in
        Lwt_result.map
          (fun selections ->
            let selections_with_command =
              selections
              |> List.map (fun selection ->
                     maybe_add_build_command ~root_pkgs selection)
            in
            `Opam_build selections_with_command)
          (solve ~root_pkgs ~pinned_pkgs ~platforms))
      (function Failure msg -> Lwt_result.fail (`Msg msg) | ex -> Lwt.fail ex)

  let type_of_dir dir =
    match Opam_monorepo.detect ~dir with
    | Some info -> `Opam_monorepo info
    | None -> `Ocaml_repo

  (** Call the solver with a request containing these packages. When it returns
      a list, it is nonempty. *)
  let solve ~root_pkgs ~pinned_pkgs ~platforms ~opam_repository_commits ~job
      ~solver =
    let platforms =
      List.map
        (fun (variant, vars) -> (Variant.to_string variant, vars))
        platforms
    in
    let opam_commits_as_strs =
      opam_repository_commits
      |> List.map (fun commit ->
             Current_git.Commit_id.(repo commit, hash commit))
    in
    let request =
      {
        Ocaml_multicore_ci_api.Worker.Solve_request.opam_repository_commits =
          opam_commits_as_strs;
        root_pkgs;
        pinned_pkgs;
        platforms;
        lower_bound = false;
      }
    in
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Backend_solver.solve solver job request ~log >|= function
    | Ok [] -> Fmt.error_msg "No solution found for any supported platform"
    | Ok x -> Ok (List.map Selection.of_worker x)
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg

  let find_opam_files ~job ~dir =
    let cmd = ("", [| "find"; "."; "-maxdepth"; "3"; "-name"; "*.opam" |]) in
    Current.Process.check_output ~cwd:dir ~cancellable:true ~job cmd
    >>!= fun output ->
    let opam_files =
      String.split_on_char '\n' output
      |> List.sort String.compare
      |> List.filter_map (function
           | "" -> None
           | path ->
               let path =
                 if Astring.String.is_prefix ~affix:"./" path then
                   Astring.String.with_range ~first:2 path
                 else path
               in
               let consider_opam_file path =
                 match Fpath.v path |> Fpath.segs with
                 | [ _file ] -> true
                 | segs when List.exists is_test_dir segs ->
                     Current.Job.log job "Ignoring test directory %S" path;
                     false
                 | _ -> true
               in
               let full_path = Filename.concat (Fpath.to_string dir) path in
               if is_empty_file full_path then (
                 Current.Job.log job "WARNING: ignoring empty opam file %S" path;
                 None)
               else if consider_opam_file path then Some path
               else None)
    in
    Lwt_result.return opam_files

  let of_dir ~solver ~job ~platforms ~opam_repository_commits ~package_name
      ?is_compiler ?compiler_commit dir =
    let is_compiler = Option.value is_compiler ~default:false in
    Current.Job.log job
      "Analysing %s: @[<v>platforms=@[%a@]@,\
       opam_repository_commits=@[%a@]@,\
       is_compiler=%a@,\
       compiler_commit=%a@]"
      package_name pp_platforms platforms
      (Fmt.list Git.Commit_id.pp)
      opam_repository_commits Fmt.bool is_compiler
      (Fmt.option Git.Commit_id.pp)
      compiler_commit;
    let solve = solve ~opam_repository_commits ~job ~solver in
    let ty = type_of_dir dir in
    find_opam_files ~job ~dir >>!= fun opam_files ->
    Analyse_ocamlformat.get_ocamlformat_source job ~opam_files ~root:dir
    >>!= fun ocamlformat_source ->
    if is_compiler || opam_files = [] then
      Current.Job.start job ~pool ~level:Current.Level.Average >>= fun () ->
      Lwt_result.return
        (dummy_analysis ~platforms ~opam_repository_commits ~package_name)
    else if List.filter is_toplevel opam_files = [] then
      let dunepath = Filename.concat (Fpath.to_string dir) "dune" in
      if Sys.file_exists dunepath then
        Current.Job.start job ~pool ~level:Current.Level.Average >>= fun () ->
        Lwt_result.return
          (dummy_analysis ~platforms ~opam_repository_commits ~package_name)
      else
        Current.Job.start job ~pool ~level:Current.Level.Average >>= fun () ->
        Lwt_result.fail (`Msg "No top-level opam or dune files found!")
    else
      (match ty with
      | `Opam_monorepo info -> Opam_monorepo.selection ~info ~solve ~platforms
      | `Ocaml_repo -> opam_selections ~solve ~job ~platforms ~opam_files dir)
      >>!= fun selections ->
      let selections =
        match compiler_commit with
        | None -> selections
        | Some _ -> remove_dev_selections ~opam_files selections
      in
      let r = { opam_files; ocamlformat_source; selections } in
      Current.Job.log job "@[<v2>Results:@,%a@]"
        Yojson.Safe.(pretty_print ~std:true)
        (to_yojson r);
      Lwt_result.return r

  let of_dir_sandmark ~job ~platforms ~opam_repository_commits ~package_name
      ?is_compiler ?compiler_commit () =
    let is_compiler = Option.value is_compiler ~default:false in
    Current.Job.log job
      "Analysing %s: @[<v>platforms=@[%a@]@,\
       opam_repository_commits=@[%a@]@,\
       is_compiler=%a@,\
       compiler_commit=%a@]"
      package_name pp_platforms platforms
      (Fmt.list Git.Commit_id.pp)
      opam_repository_commits Fmt.bool is_compiler
      (Fmt.option Git.Commit_id.pp)
      compiler_commit;
    Lwt_result.return
      (dummy_analysis ~platforms ~opam_repository_commits ~package_name)
end

let platform_to_yojson (variant, vars) =
  `Assoc
    [
      ("variant", Variant.to_yojson variant);
      ("vars", Worker.Vars.to_yojson vars);
    ]

let platforms_to_yojson platforms =
  `List (List.map platform_to_yojson platforms)

module Examine = struct
  type t = Backend_solver.t

  module Key = struct
    type t = {
      sandmark_package : string option;
      src : Current_git.Commit.t;
      compiler_commit : Current_git.Commit_id.t option;
    }

    let digest { sandmark_package; src; compiler_commit } =
      let json =
        `Assoc
          [
            ( "sandmark_package",
              match sandmark_package with None -> `Null | Some s -> `String s );
            ("src", commit_to_yojson src);
            ( "compiler_commit",
              match compiler_commit with
              | None -> `Null
              | Some cc -> commit_id_to_yojson cc );
          ]
      in
      Yojson.Safe.to_string json
  end

  module Value = struct
    type t = {
      opam_repository_commits : Current_git.Commit_id.t list;
      platforms : (Variant.t * Worker.Vars.t) list;
      is_compiler : bool;
      compiler_commit : Current_git.Commit_id.t option;
      sandmark_package : string option;
    }

    let digest
        {
          opam_repository_commits;
          platforms;
          is_compiler;
          compiler_commit;
          sandmark_package;
        } =
      let json =
        `Assoc
          [
            ("opam-repositories", commit_ids_to_yojson opam_repository_commits);
            ("platforms", platforms_to_yojson platforms);
            ("is_compiler", `Bool is_compiler);
            ( "compiler_commit",
              match compiler_commit with
              | None -> `Null
              | Some cc -> commit_id_to_yojson cc );
            ( "sandmark_package",
              match sandmark_package with None -> `Null | Some s -> `String s );
          ]
      in
      Yojson.Safe.to_string json
  end

  module Outcome = Analysis

  let id = "ci-analyse"

  let run solver job { Key.src; _ }
      {
        Value.opam_repository_commits;
        platforms;
        is_compiler;
        compiler_commit;
        sandmark_package;
      } =
    let package_name = package_name_from_commit src in
    Current.Job.start job ~pool ~level:Current.Level.Average >>= fun () ->
    Current_git.with_checkout ~job src @@ fun src ->
    match sandmark_package with
    | None ->
        Analysis.of_dir ~solver ~platforms ~opam_repository_commits ~job
          ~package_name ~is_compiler ?compiler_commit src
    | Some package_name ->
        Analysis.of_dir_sandmark ~platforms ~opam_repository_commits ~job
          ~package_name ~is_compiler ?compiler_commit ()

  let pp f (k, v) = Fmt.pf f "Analyse %s %s" (Key.digest k) (Value.digest v)
  let auto_cancel = true
  let latched = true
end

module Examine_cache = Current_cache.Generic (Examine)

let remap_platforms platforms =
  platforms |> List.map (fun { Platform.variant; vars; _ } -> (variant, vars))

let filter_invariant_platforms platforms =
  platforms
  |> List.filter (fun platform ->
         let ov = Variant.ocaml_version platform.Platform.variant in
         Ocaml_version.extra ov = None)

let first_invariant_platform platforms =
  filter_invariant_platforms platforms |> List.hd |> fun x -> [ x ]

let platforms_for_package_helper ~is_compiler platforms =
  if is_compiler then first_invariant_platform platforms else platforms

let platforms_for_package ~is_compiler ~get_is_compiler_blocklisted ~platforms
    package_name =
  platforms_for_package_helper ~is_compiler platforms
  |> List.filter (fun platform ->
         let ov = Variant.ocaml_version platform.Platform.variant in
         not (get_is_compiler_blocklisted ov package_name))

let opam_repos_for_package ~is_compiler opam_repository_commits =
  if is_compiler then [ List.hd opam_repository_commits ]
  else opam_repository_commits

let split_label = function
  | None -> ("", "")
  | Some s -> (
      match String.split_on_char '@' s with [ a; b ] -> (a, b) | _ -> (s, ""))

let examine ?label ?sandmark_package ~solver ~platforms ~opam_repository_commits
    ~is_compiler ~get_is_compiler_blocklisted ~repo src =
  let label1, label2 = split_label label in
  Current.component "Analyse@ %s@ %s" label1 label2
  |> let> src = src
     and> repo = repo
     and> opam_repository_commits = opam_repository_commits
     and> platforms = platforms in
     let package_name = Repo_url_utils.package_name_from_url repo in
     let platforms =
       platforms_for_package ~is_compiler ~get_is_compiler_blocklisted
         ~platforms package_name
       |> remap_platforms
     in
     let opam_repository_commits =
       opam_repos_for_package ~is_compiler opam_repository_commits
     in
     Examine_cache.run solver
       { sandmark_package; src; compiler_commit = None }
       {
         Examine.Value.opam_repository_commits;
         platforms;
         is_compiler;
         compiler_commit = None;
         sandmark_package;
       }

let examine_with_compiler ?label ?sandmark_package ~solver ~platforms
    ~opam_repository_commits ~compiler_commit src =
  Current.component "Analysis@ %a" Fmt.(option string) label
  |> let> src = src
     and> compiler_commit = compiler_commit
     and> opam_repository_commits = opam_repository_commits
     and> platforms = platforms in
     let platforms = first_invariant_platform platforms |> remap_platforms in
     Examine_cache.run solver
       { sandmark_package; src; compiler_commit = Some compiler_commit }
       {
         Examine.Value.opam_repository_commits;
         platforms;
         is_compiler = false;
         compiler_commit = Some compiler_commit;
         sandmark_package;
       }
