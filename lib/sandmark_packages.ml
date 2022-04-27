open Lwt.Infix
open Current.Syntax
module Git = Current_git

module Map_url = Map.Make(String)

let repo_url_main_branch repo_url =
  let (url, _) = Repo_url_utils.url_gref_from_url repo_url in
  match url  with
  | "https://github.com/OCamlPro/alt-ergo.git"
  | "https://github.com/ocaml/dune.git"
  | "https://github.com/ocaml-ppx/ppxlib.git"
  | "https://github.com/owlbarn/owl.git"
  | "https://github.com/Coquera/psmt2-frontend.git"
  | "https://github.com/CraigFe/progress.git"
  | "https://github.com/dgllghr/interval-map.git"
  | "https://github.com/mirage/either.git"
  | "https://github.com/mirage/index.git"
  | "https://github.com/mirage/irmin.git"
  | "https://github.com/mirage/semaphore-compat.git"
  | "https://github.com/mirage/repr.git" -> String.cat url "@main"
  | _ -> String.cat url "@master"

let find_opam_files path = Bos.Cmd.(v "find" % path % "-name" % "opam")

let packages_in_depends f =
  let get_atom = function OpamFormula.Atom a -> a | _ -> assert false in
  OpamFormula.ands_to_list f |> List.map get_atom

let opamfile_from_path path =
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))

let name_version_url_from_path path =
  let opam = opamfile_from_path path in
  let repo_url  url version = String.concat "@" [OpamUrl.base_url url; OpamPackage.Version.to_string version] in
  let name,version,repo = match opam.name, opam.version, opam.dev_repo with
  | Some n, Some v, Some r -> OpamPackage.Name.to_string n, OpamPackage.Version.to_string v, repo_url r v
  | Some n, Some v, None   -> OpamPackage.Name.to_string n, OpamPackage.Version.to_string v, ""
  | _,_,_ -> assert false
  in name,version,repo


let opamfile_from_opam_repository opam_repository_path name version =
  let pack = if String.equal version "" then name else [name;version] |>  String.concat "." in
  let path = [opam_repository_path;name;pack;"opam"] |> String.concat "/" in
  if Sys.file_exists path then path else ""

let dev_repo_from_opam_repository opam_repository_path name version =
  let path = opamfile_from_opam_repository opam_repository_path name version in
  if String.equal ""  path then ""
  else
    let opam = opamfile_from_path path in
    let url_repo =
      match opam.dev_repo, opam.version with
      | Some r, Some v -> String.concat "@" [OpamUrl.base_url r; OpamPackage.Version.to_string v]
      | _ -> ""
    in url_repo

let packages_in_depends_from_path path  =
  let version f =
    match f with
    | OpamFormula.Atom (OpamTypes.Constraint (`Eq, OpamTypes.FString s)) -> s
    | OpamFormula.Empty -> ""
    | _ -> assert false
  in
  let packages_from_filtered_formula formula =
    let atoms = packages_in_depends formula in
    List.map (function (pack,v) ->
      OpamPackage.Name.to_string pack, version v) atoms
  in
  packages_from_filtered_formula (opamfile_from_path path).depends

let opam_files path =
  match Bos.OS.Cmd.(run_out (find_opam_files path) |> to_lines) with
  | Ok r -> r
  | _ -> assert false

type sandmark_dep = {
  packages: string list;
  repo_url: string;
  version: string;
  main_repo_url : string;
  (*the main branch of the current that could be main,master or else, like "git_url@main/master"*)
} [@@deriving yojson]

module Op = struct
  type t = No_context
  let id = "sandmark-packages"

  module Key = struct
    type t = {
      repo_url: string;
      commit: Git.Commit.t;
    }
    let digest t =
      Fmt.str "%s / %a / extracted_packages"
        t.repo_url
        Git.Commit.pp t.commit
  end

  module Value = struct
    type t = Git.Commit.t
    let digest = Git.Commit.hash
  end

  module Outcome = struct
    type t =  {
      packages : sandmark_dep list;
      packages_missing_dev_repo : sandmark_dep list
    } [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string
    let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  end

  (**
    key : sandmark commit
    commit: opam-repository commit

    there's two(2) different opam-repository where the git url where found.
    If the url is not found in sandamrk opam-repository, it's searched in the default
    opam-repository.

  *)
  let publish No_context job (key: Key.t) (commit: Value.t) =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Git.with_checkout ~job key.commit @@ fun path ->
    let dev_opam_path = Fpath.(path / "dependencies" / "template" / "dev.opam") in
    let dev_packages_name_version = packages_in_depends_from_path (Fpath.to_string dev_opam_path) in
    let sand_opam_repository_path = Fpath.to_string (Fpath.(path / "dependencies" / "packages")) in
    let opam_files = opam_files  sand_opam_repository_path in
    let pack_packages_name_version =
      List.map (fun file_path -> name_version_url_from_path file_path) opam_files
    in
    Git.with_checkout ~job commit @@ fun path ->
    let default_opam_repository_path = Fpath.to_string Fpath.(path / "packages") in
    let dev_repo (name,version) =
      let repo = dev_repo_from_opam_repository sand_opam_repository_path name version in
      let repo =
        if String.equal repo "" then
          dev_repo_from_opam_repository default_opam_repository_path name version
        else repo
      in
      (name,version,repo)
    in
    let packages =
      pack_packages_name_version @
      List.map (fun name_version -> dev_repo name_version) dev_packages_name_version
    in
    let _ = Current.Job.log job "%s" "Founded packages:" in
    let _ = List.iter (fun (n,v,r) ->
      Current.Job.log  job
        "%s@; version= %s@; repo_url= %s"
        n (if v = "" then "None" else v) (if "" = r then "None" else r))
      packages
    in
    let new_package (name,version,repo_url)  (pack_map,pack_no_dev_repo) =
      if String.equal "" repo_url then
        pack_map,{version= version; repo_url= repo_url; main_repo_url = repo_url; packages = [name] }::pack_no_dev_repo
      else
        if Map_url.mem repo_url pack_map then
          let pack = Map_url.find repo_url pack_map in
          (
            Map_url.add repo_url {
                version= version;
                repo_url= repo_url;
                main_repo_url = repo_url;
                packages= if List.mem name pack.packages then pack.packages else pack.packages@[name]
              } pack_map
          , pack_no_dev_repo)
        else
          (Map_url.add repo_url {version= version; repo_url= repo_url; main_repo_url = repo_url; packages = [name] } pack_map
          , pack_no_dev_repo)

    in
    let _ = Current.Job.log job "Founded packages(Repeatable): %s" (string_of_int (List.length packages)) in
    let packages,packages_no_dev_repo = List.fold_left (fun map p -> new_package p map) (Map_url.empty,[]) packages in
    let packages = Map_url.fold (fun _ pack l -> pack::l) packages [] in
    Lwt.return (Ok {
      Outcome.packages= List.map (fun pack -> {pack with main_repo_url = repo_url_main_branch pack.repo_url}) packages;
      packages_missing_dev_repo=packages_no_dev_repo})

  let pp f (key, _) =
    Fmt.pf f "Extracting sandmark packages from %s" key.Key.repo_url
  let auto_cancel = true
end

module BC = Current_cache.Output(Op)


let v ~repo_url commit  opam_repository_commit =
  let label = Repo_url_utils.owner_slash_name_from_url repo_url in
  Current.component "pacakges from@.%s" label |>
  let> commit = commit
  and> opam_repository_commit = opam_repository_commit
  in
  BC.set No_context { repo_url; commit} opam_repository_commit
