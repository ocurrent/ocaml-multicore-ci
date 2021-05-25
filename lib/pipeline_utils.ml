open Current.Syntax
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()
let monthly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) ()

let commit_to_yojson commit =
  `String (Git.Commit.hash commit)

let commit_id_to_yojson commit =
  `String (Git.Commit_id.digest commit)

let commit_ids_to_yojson commits =
  `List (List.map commit_id_to_yojson commits)

let github_status_of_state ~head ~make_url result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = make_url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let index_owners owners =
  owners |> List.to_seq |> Index.Owner_set.of_seq

let get_owners installations =
  installations |> List.map Github.Installation.account

let set_active_owners owners =
  owners |> index_owners |> Index.set_active_owners

let set_active_installations installations =
  let+ installations = installations in
  installations |> get_owners |> set_active_owners;
  installations

let index_repo_names repos =
  repos |> List.to_seq |> Index.Repo_set.of_seq

let index_repos repos =
  repos |>
    List.map (fun r -> (Github.Api.Repo.id r).name) |>
    index_repo_names

let gref_hash_of_commit_id commit_id =
  let gref = Git.Commit_id.gref commit_id in
  let gref = if gref = "master" then "refs/heads/master" else gref in
  let hash = Git.Commit_id.hash commit_id in
  (gref, hash)

let gref_hash_of_commit commit =
  let commit_id = Github.Api.Commit.id commit in
  gref_hash_of_commit_id commit_id

let index_gref_hashes gref_hashes =
  gref_hashes |> List.to_seq |> Index.Ref_map.of_seq

let set_active_repos_by_owner ~owner repos =
  repos |>
    index_repos |>
    Index.set_active_repos ~owner

let set_active_repo_names ~owner repo_names =
  repo_names |>
    index_repo_names |>
    Index.set_active_repos ~owner

let set_active_repos ~installation repos =
  let+ installation = installation
  and+ repos = repos in
  let owner = Github.Installation.account installation in
  set_active_repos_by_owner ~owner repos;
  repos

let set_active_refs ~repo commits =
  let+ repo = repo
  and+ commits = commits in
  let repo = Github.Api.Repo.id repo in
  commits |>
    List.map gref_hash_of_commit |>
    index_gref_hashes |>
    Index.set_active_refs ~repo;
  commits

let set_active_refs_by_id ~repo_id commits =
  commits |>
    List.map gref_hash_of_commit_id |>
    index_gref_hashes |>
    Index.set_active_refs ~repo:repo_id

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

let summarise_builds builds =
  builds
  |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
  |> Current.map summarise

let status_of_summary = function
| Ok () -> `Passed
| Error (`Active `Running) -> `Pending
| Error (`Msg _) -> `Failed

module Owner_map = Map.Make(String)

let index_by_owner repo_urls =
  repo_urls |>
    List.fold_left (fun acc repo_url ->
      let (owner, name) = Repo_url_utils.owner_name_from_url repo_url in
      Owner_map.update owner (function
      | None -> Some [(name, repo_url)]
      | Some l -> Some ((name, repo_url) :: l)) acc
    ) Owner_map.empty

let set_github_status ~head ~make_url ~pipeline_name summary =
  summary
  |> github_status_of_state ~head ~make_url
  |> Github.Api.Commit.set_status head pipeline_name

let record_builds ~repo ~hash ~builds ~summary =
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

let record_builds_github ~commit ~builds ~summary =
  let repo = Current.map Github.Api.Commit.repo_id commit in
  let hash = Current.map Github.Api.Commit.hash commit in
  record_builds ~repo ~hash ~builds ~summary
