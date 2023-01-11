let safe_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
  | _ -> false

let check_safe s =
  if not (Astring.String.for_all safe_char s) then
    Fmt.failwith "Unsafe characters in %S" s

let for_owner_name owner name =
  check_safe owner;
  check_safe name;
  let name = Printf.sprintf "dune:%s:%s" owner name in
  Obuilder_spec.Cache.v name ~target:"/src/_build"
    ~buildkit_options:[ ("sharing", "private") ]

let for_repo repo_url =
  let owner, name = Repo_url_utils.owner_name_from_url repo_url in
  for_owner_name owner name
