let git_ext_re = Str.regexp "\\.git$"
let sanitize_re = Str.regexp "[^A-Za-z0-9-_]"

let first_two = function
| [] -> ["dummy"; "dummy"]
| [a] -> [a; "dummy"]
| a :: b :: _ -> [a; b]

let url_gref_from_url url =
  let bits = url |> String.split_on_char '@' in
  match bits with
  | [url; gref] -> (url, gref)
  | _ -> (url, "master")

let owner_name_from_url url =
  let (url, _) = url_gref_from_url url in
  let bits =
    url |>
    Str.global_replace git_ext_re "" |>
    String.split_on_char '/' |>
    List.rev |>
    first_two |>
    List.map (Str.global_replace sanitize_re "_")
  in
  match bits with
  | [name; owner] -> (owner, name)
  | _ -> assert false

let owner_slash_name_from_url url =
  let (owner, name) = owner_name_from_url url in
  owner ^ "/" ^ name

let owner_name_gref_from_url url =
  let (url, gref) = url_gref_from_url url in
  let (owner, name) = owner_name_from_url url in
  owner ^ "/" ^ name ^ "@" ^ gref

let repo_id_from_url url =
  let (url, _) = url_gref_from_url url in
  let (owner, name) = owner_name_from_url url in
  { Current_github.Repo_id.owner; name }

let package_name_from_url url =
  let (url, _) = url_gref_from_url url in
  url |>
    (String.split_on_char '/') |>
    List.rev |>
    List.hd |>
    Str.global_replace git_ext_re ""

let package_name_from_commit commit =
  let open Current_git in
  commit |>
    Commit.id |>
    Commit_id.repo |>
    package_name_from_url
