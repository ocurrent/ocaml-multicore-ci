open Lwt.Infix
open Current.Syntax
module Git = Current_git

module Map_url = Map.Make(String)

let find_opam_files path = Bos.Cmd.(v "find" % path % "-name" % "opam")

let packages_in_depends f =
  let get_atom = function OpamFormula.Atom a -> a | _ -> assert false in
  OpamFormula.ands_to_list f |> List.map get_atom

let opamfile_from_path path =
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))

let name_version_from_path path =
  let opam = opamfile_from_path path in
  let name,version = match opam.name, opam.version with
  | Some n, Some v -> OpamPackage.Name.to_string n, OpamPackage.Version.to_string v
  | _,_ -> assert false
  in name,version


let opamfile_from_opam_repository opam_repository_path name version =
  let pack = if String.equal version "" then name else [name;version] |>  String.concat "." in
  let path = [opam_repository_path;name;pack;"opam"] |> String.concat "/" in
  if Sys.file_exists path then path else ""

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
    type t = string list [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string
    let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  end

  let publish No_context job (key: Key.t) (commit: Value.t) =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current.Job.log job "Extract packages from Sandmark (%s)" (Git.Commit.hash key.commit);
    Git.with_checkout ~job commit @@ fun path ->
    let dev_opam_path = Fpath.(path / "dependencies" / "template" / "dev.opam") in
    let dev_packages_name_version = packages_in_depends_from_path (Fpath.to_string dev_opam_path) in
    let sand_opam_repository_path = Fpath.to_string (Fpath.(path / "dependencies" / "packages")) in
    let opam_files = opam_files  sand_opam_repository_path in
    let pack_packages_name_version =
      List.map (fun file_path -> name_version_from_path file_path) opam_files
    in
    let packages = dev_packages_name_version @ pack_packages_name_version in
    let new_package (name,version) pack_map =
      let name_version = if String.equal version "" then name else String.concat "." [name;version] in
      if Map_url.mem (name_version) pack_map then
        pack_map
      else
        Map_url.add name_version name_version pack_map
    in
    Current.Job.log job "%s" "Founded packages:";
    List.iter (fun (n,v) ->
      Current.Job.log  job "%s@; version= %s@ " n (if v = "" then "None" else v))
      packages;
    Current.Job.log job "Founded packages: %s" (string_of_int (List.length packages));
    let packages = List.fold_left (fun map p -> new_package p map) Map_url.empty packages in
    let packages = Map_url.fold (fun _ pack l -> pack::l) packages [] in
    Lwt.return (Ok packages)

  let pp f (key, _) =
    Fmt.pf f "Extracting sandmark packages from %s" key.Key.repo_url
  let auto_cancel = true
end

module BC = Current_cache.Output(Op)


let v ~repo_url commit =
  let label = Repo_url_utils.owner_slash_name_from_url repo_url in
  Current.component "pacakges from@.%s" label |>
  let> commit = commit
  in
  BC.set No_context { repo_url; commit} commit