open Current.Syntax
open Ocaml_multicore_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

module Repo_clone = struct
  type t = string * Git.Commit.t
  let compare = compare
  let pp f (repo, r) = Fmt.pf f "%s\n%a" repo Git.Commit.pp_short r
end

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()
let monthly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) ()

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
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://multicore.ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let set_active_installations installations =
  let+ installations = installations in
  installations
  |> List.fold_left (fun acc i -> Index.Owner_set.add (Github.Installation.account i) acc) Index.Owner_set.empty
  |> Index.set_active_owners;
  installations

let set_active_repos ~installation repos =
  let+ installation = installation
  and+ repos = repos in
  let owner = Github.Installation.account installation in
  repos
  |> List.fold_left (fun acc r -> Index.Repo_set.add (Github.Api.Repo.id r).name acc) Index.Repo_set.empty
  |> Index.set_active_repos ~owner;
  repos

let set_active_refs ~repo commits =
  let+ repo = repo
  and+ commits = commits in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    commits |> List.fold_left (fun acc commit ->
        let commit_id = Github.Api.Commit.id commit in
        let gref = Git.Commit_id.gref commit_id in
        let hash = Git.Commit_id.hash commit_id in
        Index.Ref_map.add gref hash acc
      ) Index.Ref_map.empty
  );
  commits

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
  Format.printf "package_and_selection_to_opam_spec %s" package;
  let variant_label = Variant.to_string selection.Selection.variant in
  let label = Format.sprintf "%s-%s" package variant_label in
  Spec.opam ~label ~selection ~analysis (Conf.build_mechanism_for_package package)

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

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise results =
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let opam_repository_commits = Conf.opam_repository_commits

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

let status_of_summary = function
| Ok () -> `Passed
| Error (`Active `Running) -> `Pending
| Error (`Msg _) -> `Failed

let clone_fixed_repos () =
  Conf.fixed_repos
  |> List.map (fun repo_url -> current_tagged repo_url (Git.clone ~schedule:daily repo_url))
  |> Current.list_seq

let set_github_status ~head summary =
  summary
  |> github_status_of_state ~head
  |> Github.Api.Commit.set_status head Conf.ci_pipeline_name

let record_builds ~repo ~hash ~builds ~summary : unit Current.t =
  let status =
    let+ summary = summary in
    status_of_summary summary
  in
  let+ builds = builds
  and+ repo = repo
  and+ hash = hash
  and+ status = status in
  let jobs = builds |> List.map (fun (variant, (_, job_id)) -> (variant, job_id)) in
  Index.record ~repo ~hash ~status jobs

let record_builds_github ~commit ~builds ~summary : unit Current.t =
  let repo = Current.map Github.Api.Commit.repo_id commit in
  let hash = Current.map Github.Api.Commit.hash commit in
  record_builds ~repo ~hash ~builds ~summary

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
    set_github_status ~head summary
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
