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
let build_timeout = Duration.of_hour 7

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

let pool_of_arch : OV.arch -> string = function
| `X86_64 | `I386 -> "linux-x86_64"
| `Aarch32 | `Aarch64 -> "linux-arm64"
| `Ppc64le -> "linux-ppc64"
| `S390x -> assert false

let platforms =
  let v ?(arch=`X86_64) label distro ocaml_version =
    { arch; label; builder = Builders.local; pool = pool_of_arch arch; distro; ocaml_version }
  in
  let master_distro = (DD.resolve_alias DD.master_distro :> DD.t) in
  let make_distro distro =
    let distro = (DD.resolve_alias distro :> DD.t) in
    let label = DD.latest_tag_of_distro distro in
    let tag = DD.tag_of_distro distro in
    let ov = OV.(Releases.latest |> with_just_major_and_minor) in
    if distro = master_distro then
      let arches = DD.distro_arches ov distro |> List.filter ((<>) `S390x) in
      v label tag (OV.with_variant ov (Some "flambda")) ::
      List.map (fun arch -> v ~arch label tag ov) arches
    else
      [v label tag ov]
  in
  let make_release ?arch ov =
    let distro = DD.tag_of_distro master_distro in
    let ov = OV.without_patch ov in
    v ?arch (OV.to_string ov) distro ov 
  in
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
        let ovs = List.map OV.of_string_exn ["4.12";"4.11"; "4.10"; "4.03"] in
        List.map make_release ovs @ [make_release ~arch:`I386 (List.hd ovs)]
    end
  | `Multicore ->
    let ovs = List.map OV.of_string_exn ["5.00"] in
    List.map make_release ovs

let opam_repository_repos = [
  `Mainline, ({ Github.Repo_id.owner="ocaml"; name="opam-repository" }, "master");
  `MulticoreTezos, ({ Github.Repo_id.owner="ocaml-multicore"; name="tezos-opam-repository" }, "5.00.0+trunk");
  `Sandmark, ({Github.Repo_id.owner="moyodiallo"; name="sandmark-opam-repository"}, "master")
]

let github_chosen_repos repos =
  repos |> 
  List.map (fun r ->
    let (repo, branch) = List.assoc r opam_repository_repos
    in Github.Api.Anonymous.head_of repo (`Ref ("refs/heads/" ^ branch))) |>
  Current.list_seq

let opam_repository_commits =
  let chosen_repos = match target with
  | `Mainline -> [`Mainline]
  | `Multicore -> [`Mainline]
  in
  chosen_repos |> github_chosen_repos
    
let tezos_opam_repository_commits = 
  let chosen_repos = match target with
  | `Mainline -> [`Mainline]
  | `Multicore -> [`Mainline; `MulticoreTezos]
  in
  chosen_repos |> github_chosen_repos

let sandmark_opam_repository_commits =
  let chosen_repos = match target with
  | `Mainline -> [`Mainline]
  | `Multicore -> [`Mainline]
  in
  chosen_repos |> github_chosen_repos

let fixed_repos = [
  "https://github.com/AbsInt/CompCert";
  "https://github.com/coq/coq@V8.13.2";
  "https://github.com/janestreet/async@v0.14.0";
  "https://github.com/janestreet/base@v0.14.1";
  "https://github.com/janestreet/core@v0.14.1";
  "https://github.com/janestreet/core_kernel@v0.14.1";
  "https://github.com/mirage/irmin@2.9.0";
  "https://github.com/ocaml-batteries-team/batteries-included@v3.3.0";
]

let tezos_opam_repository_repos = [
  "https://github.com/ocaml-multicore/tezos@5963aae437809881f67aee3373e5d35b5aa2348f";
  (* tezos@4.12.0+domains currently doesn't work, because it requires
   * opam 2.1 and our builds are using opam 2.0.8.
   *
   * "https://github.com/ocaml-multicore/tezos@4.12.0+domains"; *)
  "https://github.com/ocsigen/lwt@5.4.1";
  "https://github.com/ocaml-community/biniou.git@1.2.1";
]

let sandmark_opam_repository_repos  = [
  "https://github.com/ocaml-bench/sandmark.git@main"
]


let build_mechanism_for_package package =
  match package with
  | "batteries" -> `Make ["all"; "test"]
  | "CompCert" -> `Script ["sudo apt-get install -y libgmp-dev"; "opam install coq menhir"; "./configure x86_64-linux"; "make -j 4 all"; "make -C test all test"]
  | "coq" -> `Script ["sudo apt-get install -y python3"; "opam install menhir ounit2"; "./configure -local -no-ask"; "make world"; "bash -c 'export PRINT_LOGS=1; make test-suite'"]
  | "ocaml-multicore" -> `Script []
  | "tezos" -> `Script ["sudo apt-get install -y rsync git m4 build-essential patch unzip wget pkg-config libgmp-dev libev-dev libhidapi-dev libffi-dev opam jq zlib1g-dev bc autoconf software-properties-common"; "gpg --keyserver keyserver.ubuntu.com --recv-keys BA6932366A755776"; "gpg --export BA6932366A755776 | sudo apt-key add -"; "sudo add-apt-repository 'deb http://ppa.launchpad.net/deadsnakes/ppa/ubuntu bionic main'"; "sudo apt-get update"; "sudo apt-get install -y python3.9 python3.9-dev python3.9-distutils python3-pip virtualenv python3.9-venv"; "wget https://sh.rustup.rs/rustup-init.sh"; "chmod +x rustup-init.sh"; "./rustup-init.sh --profile minimal --default-toolchain 1.52.1 -y"; "opam install dune"; "curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/install-poetry.py | python3.9 -"; "python3.9 -m pip install --upgrade setuptools"; "bash -c 'source $HOME/.poetry/env; source $HOME/.cargo/env; make build-deps && eval $(opam env); PATH=\"$HOME/.local/bin:$PATH\"; make -C tests_python install-dependencies && make && make test'"]
  | _ -> `Build

let is_compiler_package package =
  match package with
  | "ocaml-multicore" -> true
  | "ocaml" -> true
  | _ -> false

(*
 * Exclude a combo of compiler version (ov) and package from the build.
 * This is used e.g. for packages that are incompatible with the syntax
 * extensions in +domains+effects compilers.
 *)
let is_compiler_blocklisted ov package =
  match OV.extra ov with
  | Some "domains+effects" -> begin
    match package with
    | "CompCert"
    | "coq" -> true
    | _ -> false
  end
  | _ -> false


let is_sandmark url = String.equal url "https://github.com/ocaml-bench/sandmark.git@main"

type conf = {
  opam_repository_commits  : Current_git.Commit_id.t list Current.t
  ; fixed_repos : string list
  ; build_mechanism_for_package: string -> [`Build | `Script of string list | `Make of string list | `Lint of [`Doc | `Fmt | `Opam]]
  ; is_compiler_package: string -> bool
  ; is_compiler_blocklisted: OV.t -> string -> bool
}

let default_conf = {
  opam_repository_commits = opam_repository_commits
  ; fixed_repos = fixed_repos
  ; build_mechanism_for_package = build_mechanism_for_package
  ; is_compiler_package = is_compiler_package
  ; is_compiler_blocklisted = is_compiler_blocklisted
}

let configs = [
  default_conf;
  {
    opam_repository_commits = tezos_opam_repository_commits
    ; fixed_repos = tezos_opam_repository_repos
    ; build_mechanism_for_package = build_mechanism_for_package
    ; is_compiler_package = is_compiler_package
    ; is_compiler_blocklisted = is_compiler_blocklisted
  };
  {
    opam_repository_commits = sandmark_opam_repository_commits
    ; fixed_repos = sandmark_opam_repository_repos
    ; build_mechanism_for_package = build_mechanism_for_package
    ; is_compiler_package = is_compiler_package
    ; is_compiler_blocklisted = is_compiler_blocklisted
  }
]
