open Current.Syntax
open Ocaml_multicore_ci
open Pipeline_utils

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let opam_repository_commits = Conf.opam_repository_commits

let is_compiler_from_repo_url repo_url =
  let package_name = Repo_url_utils.package_name_from_url repo_url in
  Conf.is_compiler_package package_name

let platforms =
  let schedule = monthly in
  let v { Conf.label; builder; pool; distro; ocaml_version; arch } =
    let base = Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version in
    let host_base =
      match arch with
      | `X86_64 -> base
      | _ -> Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro ~ocaml_version
    in
    Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base base
  in
  Current.list_seq (List.map v Conf.platforms)

(* Link for GitHub statuses. *)
let make_url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://multicore.ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let remove_version_re = Str.regexp "\\..*$"

let build_mechanism_for_selection ~selection =
    let mechanisms = selection.Selection.packages |> List.map (fun package ->
        let package_raw = Str.global_replace remove_version_re "" package in
        (package, Conf.build_mechanism_for_package package_raw)
    ) in
    let (_, others) = mechanisms |> List.partition (fun (_, mechanism) -> mechanism = `Build) in
    match others with
    | [] -> `Build
    | [(_, (`Make _ as mech))] -> mech
    | [(_, (`Script _ as mech))] -> mech
    | _ -> `Build

let selection_to_opam_spec ~analysis selection =
  let label = Variant.to_string selection.Selection.variant in
  let build_mechanism = build_mechanism_for_selection ~selection in
  Spec.opam ~label ~selection ~analysis build_mechanism

let package_and_selection_to_opam_spec ~analysis ~package selection =
  let label = Variant.to_string selection.Selection.variant in
  let build_mechanism = Conf.build_mechanism_for_package package in
  Spec.opam ~label ~selection ~analysis build_mechanism

let make_opam_specs analysis =
  match Analyse.Analysis.selections analysis with
  | `Not_opam (package, selections) ->
    selections |> List.map (package_and_selection_to_opam_spec ~analysis ~package)
  | `Opam_monorepo config ->
    let lint_selection = Opam_monorepo.selection_of_config config in
    [
      Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);
      Spec.opam_monorepo ~config
    ]
  | `Opam_build selections ->
(*    let lint_selection = List.hd selections in*)
    let builds =
      selections |> List.map (selection_to_opam_spec ~analysis)
    and lint =
      [
(*        Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);*)
(*        Spec.opam ~label:"(lint-doc)" ~selection:lint_selection ~analysis (`Lint `Doc);*)
(*        Spec.opam ~label:"(lint-opam)" ~selection:lint_selection ~analysis (`Lint `Opam);*)
      ]
    in
    lint @ builds

let place_build ~ocluster ~repo ?compiler_commit ~source spec =
  let+ result =
    match ocluster with
    | None ->
      Build.v ~platforms ~repo ?compiler_commit ~spec source
    | Some ocluster ->
      let src = Current.map Git.Commit.id source in
      let compiler_commit_id = Option.map (fun c -> Current.map Git.Commit.id c) compiler_commit in
      Cluster_build.v ocluster ~platforms ~repo ?compiler_commit:compiler_commit_id ~spec src
  and+ spec = spec in
  Spec.label spec, result

let build_with_docker ?ocluster ~repo ?compiler_commit ?label ~analysis source =
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis ->
      make_opam_specs analysis
  in
  let+ builds = specs |> Current.list_map ?label (module Spec) (place_build ~ocluster ~repo ?compiler_commit ~source)
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, analysis_id);
  ]

let analysis_component ?label ~solver ~is_compiler commit =
  Analyse.examine ?label ~solver ~platforms ~opam_repository_commits ~is_compiler commit

let local_test ?label ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" } in
  let repo_str = Current.map (Fmt.to_to_string Current_github.Repo_id.pp) repo in
  let analysis = analysis_component ?label ~solver ~is_compiler:false src in
  Current.component "summarise" |>
  let> results = build_with_docker ~repo:repo_str ?label ~analysis src in
  let result =
    results
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current_incr.const (result, None)

let local_test_multiple ~solver repos () =
  repos |> List.map (fun repo ->
    let label = Git.Local.repo repo |> Fpath.basename in
    local_test ~label ~solver repo ()
  ) |> Current.all

let clone_fixed_repos (): (string * Git.Commit.t Current.t) list =
  let repos_by_owner =
    Conf.fixed_repos |> index_by_owner |> Owner_map.bindings in
  repos_by_owner |> List.split |> fst |> set_active_owners;
  repos_by_owner |> List.map (fun (owner, repo_names_urls) ->
    let (repo_names, repo_urls) = repo_names_urls |> List.split in
    set_active_repo_names ~owner repo_names;
    repo_urls |> List.map (fun repo_url ->
      let (url, gref) = Repo_url_utils.url_gref_from_url repo_url in
      (url, (Git.clone ~schedule:daily ~gref url))
    )
  ) |> List.flatten

let analyse_build_summarise ?ocluster ~solver ~repo ~is_compiler ?compiler_commit ?label commit =
  let analysis = analysis_component ~solver ?label ~is_compiler commit in
  let builds = build_with_docker ?ocluster ~repo ?compiler_commit ?label ~analysis commit in
  (builds, summarise_builds builds)

let fetch_analyse_build_summarise ?ocluster ~solver ~repo ?label head =
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let (builds, summary) = analyse_build_summarise ?ocluster ~solver ~repo ~is_compiler:false ?label src in
  let index = record_builds_github ~commit:head ~builds ~summary in
  Current.all [
    index;
    set_github_status ~head ~make_url ~pipeline_name:Conf.ci_pipeline_name
      summary
  ]

let build_from_clone_with_compiler ?ocluster ~solver ?compiler_commit ?compiler_build repo_clone =
  let (repo_url, commit) = repo_clone in
  let commit = Build_from_clone_component.v ~repo_url ?compiler_commit ?dependency:compiler_build commit in
  let repo_id = Repo_url_utils.repo_id_from_url repo_url in
  let hash = Current.map Git.Commit.hash commit in
  let label = Repo_url_utils.owner_slash_name_from_url repo_url in
  let is_compiler = is_compiler_from_repo_url repo_url in
  let (builds, summary) = analyse_build_summarise ?ocluster ~solver ~is_compiler ?compiler_commit ~label ~repo:(Current.return repo_url) commit in
  record_builds ~repo:(Current.return repo_id) ~hash ~builds ~summary

let build_from_clone ?ocluster ~solver (repo_clone: (string * Git.Commit.t Current.t)) =
  let (repo_url, commit) = repo_clone in
  if is_compiler_from_repo_url repo_url
  then
    let compiler_build =
      build_from_clone_with_compiler ?ocluster ~solver
        ~compiler_commit:commit repo_clone
    in
    let downstream_builds = clone_fixed_repos () |>
      List.filter_map (fun child_repo_clone ->
        let (child_repo_url, _) = child_repo_clone in
        if is_compiler_from_repo_url child_repo_url then
          None
        else
          Some (
            build_from_clone_with_compiler ?ocluster ~solver
              ~compiler_commit:commit ~compiler_build child_repo_clone
            )
      )
    in
    Current.all (compiler_build :: downstream_builds)
  else
    build_from_clone_with_compiler ?ocluster ~solver repo_clone

let build_installation ?ocluster ~solver installation =
  let repos = Github.Installation.repositories installation |> set_active_repos ~installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo |> set_active_refs ~repo in
  refs |> Current.list_iter (module Github.Api.Commit) @@ (fetch_analyse_build_summarise ?ocluster ~solver ~repo:(Current.map (Fmt.to_to_string Github.Api.Repo.pp) repo))

let v ?ocluster ~app ~solver () =
  let ocluster = Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster in
  Current.with_context opam_repository_commits @@ fun () ->
  Current.with_context platforms @@ fun () ->
  let build_installations = match app with
  | None -> Current.return ()
  | Some app ->
    let installations = Github.App.installations app |> set_active_installations in
    installations |> Current.list_iter
      ~label:"GitHub installations" ~collapse_key:"org"
      (module Github.Installation) @@ (build_installation ?ocluster ~solver)
  in
  let build_fixed =
    clone_fixed_repos () |> List.map (build_from_clone ?ocluster ~solver)
  in
  Current.all (build_installations :: build_fixed)

let local_test_fixed ~solver (): unit Current.t =
  Current.with_context opam_repository_commits @@ fun () ->
  Current.with_context platforms @@ fun () ->
  clone_fixed_repos () |> List.map (build_from_clone ~solver) |> Current.all
