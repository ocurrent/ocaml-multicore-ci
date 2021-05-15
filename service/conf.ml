let ci_pipeline_name = "ocaml-multicore-ci"

let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $CI_PROFILE setting %S" x

let target =
  match Sys.getenv_opt "CI_TARGET" with
  | Some "multicore" -> `Multicore
  | Some "mainline" | None -> `Mainline
  | Some x -> Fmt.failwith "Unknown $CI_TARGET setting %S" x

(* GitHub defines a stale branch as more than 3 months old.
   Don't bother testing these. *)
let max_staleness = Duration.of_day 93

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match profile with
    | `Production -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/ocaml-ci-admin.cap"
  let internal_port = 9000
end

let dev_pool = Current.Pool.create ~label:"docker" 1

(** Maximum time for one Docker build. *)
let build_timeout = Duration.of_hour 1

module Builders = struct
  let v docker_context =
    let docker_context, pool =
      Some docker_context, Current.Pool.create ~label:("docker-" ^ docker_context) 20
    in
    { Ocaml_multicore_ci.Builder.docker_context; pool; build_timeout }

  let local = { Ocaml_multicore_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

module OV = Ocaml_version
module DD = Dockerfile_distro
module Github = Current_github

let default_compiler = OV.(Releases.latest |> without_patch)
let trunk_compiler = OV.(Sources.trunk |> without_patch)

type platform = {
  label : string;
  builder : Ocaml_multicore_ci.Builder.t;
  pool : string;
  distro : string;
  ocaml_version : OV.t;
  arch: OV.arch;
}

let pool_of_arch = function
| `X86_64 | `I386 -> "linux-x86_64"
| `Aarch32 | `Aarch64 -> "linux-arm64"
| `Ppc64le -> "linux-ppc64"

let platforms =
  let v ?(arch=`X86_64) label distro ocaml_version =
    { arch; label; builder = Builders.local; pool = pool_of_arch arch; distro; ocaml_version }
  in
  let master_distro = DD.resolve_alias DD.master_distro in
  let make_distro distro =
    let distro = DD.resolve_alias distro in
    let label = DD.latest_tag_of_distro distro in
    let tag = DD.tag_of_distro distro in
    let ov = OV.(Releases.latest |> with_just_major_and_minor) in
    if distro = master_distro then
      v label tag (OV.with_variant ov (Some "flambda")) ::
      List.map (fun arch -> v ~arch label tag ov) (DD.distro_arches ov distro)
    else
      [v label tag ov]
  in
  let make_release ?arch ov =
    let distro = DD.tag_of_distro master_distro in
    let ov = OV.without_patch ov in
    v ?arch (OV.to_string ov) distro ov in
  match target with
  | `Mainline -> begin
    match profile with
    | `Production ->
        let distros =
          DD.active_tier1_distros `X86_64 @ DD.active_tier2_distros `X86_64 |>
          List.map make_distro |> List.flatten in
        (* The first one in this list is used for lint actions *)
        let ovs = List.rev OV.Releases.recent @ OV.Releases.unreleased_betas in
        List.map make_release ovs @ distros
    | `Dev ->
        let ovs = List.map OV.of_string_exn ["4.11"; "4.10"; "4.03"] in
        List.map make_release ovs @ [make_release ~arch:`I386 (List.hd ovs)]
    end
  | `Multicore ->
    let ovs = List.map OV.of_string_exn ["4.10+multicore"; "4.12"; "4.12+domains"; "4.12+domains+effects"] in
    List.map make_release ovs

let opam_repository_repos = [
  `Mainline, { Github.Repo_id.owner="ocaml"; name="opam-repository" };
  `Multicore, { Github.Repo_id.owner="ocaml-multicore"; name="multicore-opam" }
]

let opam_repository_commits =
  let chosen_repos = match target with
  | `Mainline -> [`Mainline]
  | `Multicore -> [`Mainline; `Multicore]
  in
  chosen_repos |>
    List.map (fun r -> Github.Api.Anonymous.head_of (List.assoc r opam_repository_repos) (`Ref "refs/heads/master")) |>
    Current.list_seq

let fixed_repos = [
  "https://github.com/AbsInt/CompCert";
  "https://github.com/coq/coq";
  "https://github.com/janestreet/async";
  "https://github.com/janestreet/base";
  "https://github.com/janestreet/core";
  "https://github.com/janestreet/core_kernel";
  "https://github.com/ocaml-batteries-team/batteries-included";
  "https://github.com/ocsigen/lwt";
]

let build_mechanism_for_package package =
  match package with
  | "batteries" -> `Make ["all"; "test"]
  | "CompCert" -> `Script ["sudo apt-get install -y libgmp-dev"; "opam install coq"; "./configure x86_64-linux"; "make"; "make test"]
  | "coq" -> `Script ["./configure -no-ask"; "make"; "make test"]
  | _ -> `Build
