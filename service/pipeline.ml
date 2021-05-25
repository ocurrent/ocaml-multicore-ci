open Current.Syntax
open Ocaml_multicore_ci
open Pipeline_utils

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let opam_repository_commits = Conf.opam_repository_commits

module Repo_clone = struct
  type t = string * Git.Commit.t
  let compare = compare
  let pp f (repo, r) = Fmt.pf f "%s\n%a" repo Git.Commit.pp_short r
end

let current_tagged a b =
  Current.pair (Current.return a) b

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

let place_build ~ocluster ~repo ~source spec =
  let+ result =
    match ocluster with
    | None ->
      Build.v ~platforms ~repo ~spec source
    | Some ocluster ->
      let src = Current.map Git.Commit.id source in
      Cluster_build.v ocluster ~platforms ~repo ~spec src
  and+ spec = spec in
  Spec.label spec, result

let build_with_docker ?ocluster ~repo ?label ~analysis source =
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
  let+ builds = specs |> Current.list_map ?label (module Spec) (place_build ~ocluster ~repo ~source)
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, analysis_id);
  ]

let local_test ?label ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" } in
  let repo_str = Current.map (Fmt.to_to_string Current_github.Repo_id.pp) repo in
  let analysis = Analyse.examine ?label ~solver ~platforms ~opam_repository_commits src in
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

let clone_fixed_repos () =
  Conf.fixed_repos
  |> List.map (fun repo_url -> current_tagged repo_url (Git.clone ~schedule:daily repo_url))
  |> Current.list_seq

let analyse ~solver commit =
  Analyse.examine ~solver ~platforms ~opam_repository_commits commit

let summarise_builds builds =
  builds
  |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
  |> Current.map summarise

let analyse_build_summarise ?ocluster ~solver ~repo commit =
  let analysis = analyse ~solver commit in
  let builds = build_with_docker ?ocluster ~repo ~analysis commit in
  (builds, summarise_builds builds)

let fetch_analyse_build_summarise ?ocluster ~solver ~repo head =
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let (builds, summary) = analyse_build_summarise ?ocluster ~solver ~repo src in
  let index = record_builds_github ~commit:head ~builds ~summary in
  Current.all [
    index;
    set_github_status ~head ~make_url ~pipeline_name:Conf.ci_pipeline_name
      summary
  ]

let build_installation ?ocluster ~solver installation =
  let repos = Github.Installation.repositories installation |> set_active_repos ~installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo |> set_active_refs ~repo in
  refs |> Current.list_iter (module Github.Api.Commit) @@ (fetch_analyse_build_summarise ?ocluster ~solver ~repo:(Current.map (Fmt.to_to_string Github.Api.Repo.pp) repo))

let build_from_clone ?ocluster ~solver repo_clone =
  let repo_url = Current.map fst repo_clone in
  let commit = Current.map snd repo_clone in
  let hash = Current.map Git.Commit.hash commit in
  let repo_id = Current.map Repo_url_utils.repo_id_from_url repo_url in
  let (builds, summary) = analyse_build_summarise ?ocluster ~solver ~repo:repo_url commit in
  record_builds ~repo:repo_id ~hash ~builds ~summary

let v ?ocluster ~app ~solver () =
  let ocluster = Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster in
  Current.with_context opam_repository_commits @@ fun () ->
  Current.with_context platforms @@ fun () ->
  let installations = Github.App.installations app |> set_active_installations in
  let build_installations =
    installations |> Current.list_iter ~collapse_key:"org" (module Github.Installation) @@ (build_installation ?ocluster ~solver)
  in
  let build_fixed =
    clone_fixed_repos () |> Current.list_iter (module Repo_clone) (build_from_clone ?ocluster ~solver)
  in
  Current.all [
    build_installations;
    build_fixed;
  ]

let local_test_fixed ~solver (): unit Current.t =
  Current.with_context opam_repository_commits @@ fun () ->
  Current.with_context platforms @@ fun () ->
  clone_fixed_repos () |> Current.list_iter (module Repo_clone) (build_from_clone ~solver)
