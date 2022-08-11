open Lwt.Infix
module Client = Ocaml_multicore_ci_api.Client
module Capability = Capnp_rpc_lwt.Capability

let (>>!=) x f =
  x >>= function
  | Error `Capnp ex -> Dream.respond ~status:`Internal_Server_Error (Fmt.to_to_string Capnp_rpc.Error.pp ex)
  | Ok y -> f y

let org_url owner =
  Printf.sprintf "/github/%s" owner

let repo_url ~owner name =
  Printf.sprintf "/github/%s/%s" owner name

module Build_status = struct
  include Client.Build_status

  let class_name (t : t) =
    match t with
    | NotStarted -> "not-started"
    | Failed -> "failed"
    | Passed -> "passed"
    | Pending -> "active"
    | Undefined _ -> "undefined"
end

let format_org org =
  let open Tyxml.Html in
  li [a ~a:[a_href (org_url org)] [txt org]]

let breadcrumbs steps page_title =
  let open Tyxml.Html in
  let add (prefix, results) (label, link) =
    let prefix = Printf.sprintf "%s/%s" prefix link in
    let link = li [a ~a:[a_href prefix] [txt label]] in
    (prefix, link :: results)
  in
  let _, steps = List.fold_left add ("", []) steps in
  let steps = li [b [txt page_title]] :: steps in
  ol ~a:[a_class ["breadcrumbs"]] (
      List.rev steps
    )

let commit_url ~owner ~name hash =
  Printf.sprintf "/github/%s/%s/commit/%s" owner name hash


let format_refs ~owner ~name refs =
  let open Tyxml.Html in
  ul ~a:[a_class ["statuses"]] (
      Client.Ref_map.bindings refs 
      |> List.map @@ fun (branch, (commit, status)) ->
                     li ~a:[a_class [Build_status.class_name status]] [
                         a ~a:[a_href (commit_url ~owner ~name commit)] [txt branch]
                       ]
    )

let orgs ci = 
  Client.CI.orgs ci >>!= fun orgs ->
  let body = Template.instance Tyxml.Html.[
        breadcrumbs [] "github";
        ul (List.map format_org orgs)
  ] in
  Dream.respond body

let format_repo ~owner { Client.Org.name; master_status } =
  let open Tyxml.Html in
  li ~a:[a_class [Build_status.class_name master_status]] [
      a ~a:[a_href (repo_url ~owner name)] [txt name]
    ]

let list_repos ~owner org =
  Client.Org.repos org >>!= fun repos ->
  let body = Template.instance Tyxml.Html.[
        breadcrumbs ["github", "github"] owner;
        ul ~a:[a_class ["statuses"]] (List.map (format_repo ~owner) repos)
  ] in
  Dream.respond body

let list_repos ~owner ci =
  Capability.with_ref (Client.CI.org ci owner) @@ list_repos ~owner

let repo ~owner ~name ci = 
  Capability.with_ref (Client.CI.org ci owner) @@ fun org ->
  Capability.with_ref (Client.Org.repo org name) @@ fun repo ->
  Client.Repo.refs repo >>!= fun refs ->
    let body = Template.instance [
                   breadcrumbs ["github", "github";
                                owner, owner] name;
                   format_refs ~owner ~name refs
                 ] in
    Dream.respond body