let host_network = ["host"]
let opam_download_cache = [ Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache" ]

let run fmt =
  let network = host_network in
  let cache = opam_download_cache in
  Obuilder_spec.run ~network ~cache fmt

let compiler_switch_name_from_commit commit =
  let s_hash =
    Current_git.Commit_id.hash commit |> Astring.String.with_range ~len:8
  in
  "compiler-" ^ s_hash

(* If the package's directory name doesn't contain a dot then opam will default to
   using the last known version, which is usually wrong. In particular, if a multi-project
   repostory adds a new package with a constraint "{ =version }" on an existing one,
   this will fail because opam will pin the new package as "dev" but the old one with
   the version of its last release. *)
let maybe_add_dev ~dir name =
  if Fpath.is_current_dir dir || not (String.contains (Fpath.basename dir) '.') then name ^ ".dev" else name

(* Group opam files by directory.
   e.g. ["a/a1.opam"; "a/a2.opam"; "b/b1.opam"] ->
        [("a", ["a/a1.opam"; "a/a2.opam"], ["a1.dev"; "a2.dev"]);
         ("b", ["b/b1.opam"], ["b1.dev"])
        ] *)
let group_opam_files =
  ListLabels.fold_left ~init:[] ~f:(fun acc x ->
      let item = Fpath.v x in
      let dir = Fpath.parent item in
      let pkg = Filename.basename x |> Filename.chop_extension |> maybe_add_dev ~dir in
      match acc with
      | (prev_dir, prev_items, pkgs) :: rest when Fpath.equal dir prev_dir -> (prev_dir, x :: prev_items, pkg :: pkgs) :: rest
      | _ -> (dir, [x], [pkg]) :: acc
    )

let mkdir dirs =
  let dirs =
    dirs
    |> List.map (fun (dir, _, _) -> Filename.quote (Fpath.to_string dir))
    |> String.concat " "
  in
  [run "mkdir -p %s" dirs]

(* Generate instructions to copy all the files in [items] into the
   image, creating the necessary directories first, and then pin them all. *)
let pin_opam_files groups =
  if groups = [] then
    []
  else
    let open Obuilder_spec in
    let cmds = mkdir groups @ (
      groups |> List.map (fun (dir, files, _) ->
          copy files ~dst:(Fpath.to_string dir)
        )
    ) @ [
      groups |> List.concat_map (fun (dir, _, pkgs) ->
          pkgs
          |> List.map (fun pkg ->
              Printf.sprintf "opam pin add -yn %s %s" pkg (Filename.quote (Fpath.to_string dir))
            )
        )
      |> String.concat " && \n"
      |> run "%s"
    ]
    in
    comment "Pin project opam files" :: workdir "/src" :: cmds

(* Get the packages directly in "." *)
let rec get_root_opam_packages = function
  | [] -> []
  | (dir, _, pkgs) ::_ when Fpath.is_current_dir dir -> pkgs
  | _ :: rest -> get_root_opam_packages rest

let install_deps ~groups ~selection =
  let { Selection.packages; _ } = selection in
  let root_pkgs = get_root_opam_packages groups in
  let non_root_pkgs = packages |> List.filter (fun pkg -> not (List.mem pkg root_pkgs)) in
  let open Obuilder_spec in
  let non_root_pkgs_str = String.concat " " non_root_pkgs in
  let root_pkgs_str = String.concat " " root_pkgs in
  let install_cmds = match non_root_pkgs with
  | [] ->
    [
      run "opam depext --update -y %s" root_pkgs_str
    ]
  | _ ->
    [
      env "DEPS" non_root_pkgs_str;
      run "opam depext --update -y %s $DEPS" root_pkgs_str;
      run "opam install $DEPS"
    ]
  in
  comment "Install opam deps" :: install_cmds

let install_os_deps selection =
  let { Selection.variant; _ } = selection in
  let open Obuilder_spec in
  let linux32 = if Variant.arch variant |> Ocaml_version.arch_is_32bit then
    [shell ["/usr/bin/linux32"; "/bin/sh"; "-c"]]
  else
    []
  in
  let distro_extras = if Astring.String.is_prefix ~affix:"fedora" (Variant.id variant) then
    [run "sudo dnf install -y findutils"] (* (we need xargs) *)
  else
    []
  in
  let result = linux32 @ distro_extras in
  if result = [] then
    []
  else
    comment "Preamble" :: result

let update_opam_repository selection =
  let { Selection.commit; _ } = selection in
  let open Obuilder_spec in
  [
    comment "Update opam-repository";
    workdir "/home/opam/opam-repository";
    run
      "(git cat-file -e %s || git fetch origin master) && \
       git reset -q --hard %s && git log --no-decorate -n1 --oneline \
       && opam update -u" commit commit;
  ]

let copy_src =
  let open Obuilder_spec in
  [
    comment "Initialize project source";
    copy ["."] ~dst:"/src/";
    workdir "/src";
  ]

let pin_and_install_deps ~opam_files selection =
  let groups = group_opam_files opam_files in
  pin_opam_files groups @
    install_deps ~groups ~selection

let install_project_deps ~opam_files ~selection =
  install_os_deps selection @
    update_opam_repository selection @
    pin_and_install_deps ~opam_files selection

let install_compiler commit =
  let switch_name = compiler_switch_name_from_commit commit in
  let open Obuilder_spec in
  [
    comment "Create switch for compiler (%s)" switch_name;
    run "opam switch create %s --empty && opam repository && opam pin add -y -k path --inplace-build ocaml-variants.4.12.0+multicore . && eval $(opam env) && ocamlrun -version" switch_name
  ]

let print_compiler_version =
  run "eval $(opam env) && opam switch && ocamlrun -version"

let spec_helper ~body ~base ~opam_files ~compiler_commit ~selection =
  let open Obuilder_spec in
  stage ~from:base ([
    comment "Variant: %s" (Fmt.strf "%a" Variant.pp selection.Selection.variant);
    user ~uid:1000 ~gid:1000;
  ] @
    install_os_deps selection @
    update_opam_repository selection @
    (match compiler_commit with
    | None ->
      print_compiler_version ::
        pin_and_install_deps ~opam_files selection @
        copy_src
    | Some compiler_commit ->
      copy_src @
        install_compiler compiler_commit
    ) @
    body
  )

let spec_script ~base ~opam_files ~compiler_commit ~selection ~cmds =
  let cmds = cmds |> List.map (fun cmd ->
    run "opam exec -- %s" cmd
  ) in
  let body = Obuilder_spec.comment "Run build" :: cmds in
  spec_helper ~body ~base ~opam_files ~compiler_commit ~selection

let spec_dune ~base ~opam_files ~compiler_commit ~selection =
  let cmd = match selection.Selection.command with
  | Some c -> c
  | None -> "dune build @install @runtest"
  in
  let cmds = [cmd ^ " && rm -rf _build"] in
  spec_script ~base ~opam_files ~compiler_commit ~selection ~cmds

let spec_opam_install ~base ~opam_files ~compiler_commit ~selection =
  let opam_packages =
    opam_files |> List.map Filename.chop_extension
  in
  let cmds = [
    Fmt.str "opam install %a" Fmt.(list ~sep:(unit " ") string) opam_packages
  ] in
  spec_script ~base ~opam_files ~compiler_commit ~selection ~cmds

let spec_make ~base ~opam_files ~compiler_commit ~selection ~targets =
  let cmds = [Format.sprintf "make %s" (String.concat " " targets)] in
  spec_script ~base ~opam_files ~compiler_commit ~selection ~cmds
