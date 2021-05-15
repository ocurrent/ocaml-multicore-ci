let git_ext_re = Str.regexp "\\.git$"
let sanitize_re = Str.regexp "[^A-Za-z0-9-_]"

let first_two = function
| [] -> ["dummy"; "dummy"]
| [a] -> [a; "dummy"]
| a :: b :: _ -> [a; b]

let owner_name_from_url url =
  let bits =
    url |>
    Str.global_replace git_ext_re "" |>
    String.split_on_char '/' |>
    List.rev |>
    first_two |>
    List.map (Str.global_replace sanitize_re "_")
  in
  match bits with
  | [owner; name] -> (owner, name)
  | _ -> assert false

let repo_id_from_url url =
  let (owner, name) = owner_name_from_url url in
  { Current_github.Repo_id.owner; name }
