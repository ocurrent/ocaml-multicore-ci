open Graphql_lwt
module Client = Ocaml_multicore_ci_api.Client

type build_status = Ocaml_multicore_ci_api.Raw.Reader.BuildStatus.t

type job_state =
  | NotStarted
  | Passed
  | Failed
  | Active
  | Aborted
  | Undefined

module Org = struct
  type t = {
    org: Client.Org.t;
    name: string;
  }
end

module Repo_info = struct
  type t = {
    repo: Client.Repo.t;
    name: string;
    master_status: build_status;
  }
end

module Ref_info = struct
  type t = {
    commit: Client.Commit.t;
    gref: string;
    hash: string;
    status: build_status;
  }
end

module Job_info = struct
  type t = {
    owner : string;
    name : string;
    hash : string;
    variant : string;
    outcome : job_state;
    error : string option;
  }
end

let lwt_result_map_list f result =
  Lwt_result.map (fun rs -> List.map f rs) result

let remap_capnp_err result
 = Lwt_result.map_err (function `Capnp err -> Fmt.to_to_string Capnp_rpc.Error.pp err) result

let convert_job_state (x: Client.State.t) =
match x with
| NotStarted -> NotStarted
| Passed -> Passed
| Failed _ -> Failed
| Active -> Active
| Aborted -> Aborted
| Undefined _ -> Undefined

let error_of_job_state (x: Client.State.t) =
match x with
| Failed err -> Some err
| Undefined err -> Some (Int.to_string err)
| _ -> None

let list_jobs commit : (Job_info.t list, string) Lwt_result.t =
  Client.Commit.jobs commit
    |> remap_capnp_err
    |> lwt_result_map_list (fun job ->
       let orig_outcome = job.Client.outcome in
       let outcome = convert_job_state orig_outcome in
       let error = error_of_job_state orig_outcome in
       {Job_info.owner=""; name=""; hash=""; variant=job.Client.variant; outcome; error }
    )

let list_all_jobs ci : (Job_info.t list, string) Lwt_result.t =
  Client.CI.jobs ci
    |> remap_capnp_err
    |> lwt_result_map_list (fun job ->
       let orig_outcome = job.Client.outcome in
       let outcome = convert_job_state orig_outcome in
       let error = error_of_job_state orig_outcome in
       {Job_info.owner=job.Client.owner; name=job.Client.name; hash=job.Client.hash; variant=job.Client.variant; outcome; error }
    )

let list_refs repo : (Ref_info.t list, string) Lwt_result.t =
  Client.Repo.refs repo
    |> remap_capnp_err
    |> Lwt_result.map @@ fun refs ->
  refs
    |> Client.Ref_map.mapi (fun gref (hash, status) ->
       let commit = Client.Repo.commit_of_hash repo hash in
       { Ref_info.commit; hash; gref; status })
    |> Client.Ref_map.bindings |> List.split |> snd

let list_repos org : (Repo_info.t list, string) Lwt_result.t =
  Client.Org.repos org
    |> remap_capnp_err
    |> lwt_result_map_list (fun repo_info ->
       let repo = Client.Org.repo org repo_info.Client.Org.name in
       { Repo_info.repo; name=repo_info.Client.Org.name; master_status=repo_info.Client.Org.master_status }
    )

let list_orgs ci : (Org.t list, string) Lwt_result.t =
  Client.CI.orgs ci
    |> remap_capnp_err
    |> lwt_result_map_list (fun name ->
       let org = Client.CI.org ci name in
       { Org.name; org }
    )

let build_status = Ocaml_multicore_ci_api.Raw.Reader.BuildStatus.(Schema.(enum "build_status"
  ~values:[
    enum_value "NotStarted" ~value:NotStarted;
    enum_value "Passed" ~value:Passed;
    enum_value "Failed" ~value:Failed;
    enum_value "Pending" ~value:Pending;
  ]
))

let job_state = Schema.(enum "job_state"
  ~values:[
    enum_value "NotStarted" ~value:NotStarted;
    enum_value "Passed" ~value:Passed;
    enum_value "Failed" ~value:Failed;
    enum_value "Active" ~value:Active;
    enum_value "Aborted" ~value:Aborted;
  ]
)

let job_info = Schema.(obj "job_info" ~fields:(fun _ -> [
  field "owner"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Job_info.owner);
  field "name"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Job_info.name);
  field "hash"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Job_info.hash);
  field "variant"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Job_info.variant);
  field "outcome"
    ~args:Arg.[]
    ~typ:(non_null job_state)
    ~resolve:(fun _ p -> p.Job_info.outcome);
  field "error"
    ~args:Arg.[]
    ~typ:string
    ~resolve:(fun _ p -> p.Job_info.error);
]))

let ref_info = Schema.(obj "ref_info" ~fields:(fun _ -> [
  field "gref"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Ref_info.gref);
  field "hash"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Ref_info.hash);
  field "status"
    ~args:Arg.[]
    ~typ:(non_null build_status)
    ~resolve:(fun _ p -> p.Ref_info.status);
  io_field "jobs"
    ~args:Arg.[]
    ~typ:(non_null (list (non_null job_info)))
    ~resolve:(fun _ p -> list_jobs p.Ref_info.commit);
]))

let repo_info = Schema.(obj "repo_info" ~fields:(fun _ -> [
  field "name"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Repo_info.name);
  field "master_status"
    ~args:Arg.[]
    ~typ:(non_null build_status)
    ~resolve:(fun _ p -> p.Repo_info.master_status);
  io_field "refs"
    ~args:Arg.[]
    ~typ:(non_null (list (non_null ref_info)))
    ~resolve:(fun _ p -> list_refs p.Repo_info.repo);
]))

let org = Schema.(obj "org" ~fields:(fun _ -> [
  field "name"
    ~args:Arg.[]
    ~typ:(non_null string)
    ~resolve:(fun _ p -> p.Org.name);
  io_field "repos"
    ~args:Arg.[]
    ~typ:(non_null (list (non_null repo_info)))
    ~resolve:(fun _ p -> list_repos p.Org.org);
]))

let schema ci =
  Schema.(
    schema
      [
        io_field "orgs"
          ~args:Arg.[]
          ~typ:(non_null (list (non_null org)))
          ~resolve:(fun _ () -> list_orgs ci);
        io_field "jobs"
          ~args:Arg.[]
          ~typ:(non_null (list (non_null job_info)))
          ~resolve:(fun _ () -> list_all_jobs ci);
      ]
      ~subscriptions:[])
