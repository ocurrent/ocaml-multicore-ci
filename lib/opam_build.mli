val opam_download_cache : Obuilder_spec.Cache.t list

val install_project_deps :
  opam_files:string list -> selection:Selection.t -> Obuilder_spec.op list

val spec_dune :
  repo:string option ->
  base:string ->
  opam_files:string list ->
  compiler_commit:Current_git.Commit_id.t option ->
  selection:Selection.t ->
  Obuilder_spec.t

val spec_opam_install :
  base:string ->
  opam_files:string list ->
  compiler_commit:Current_git.Commit_id.t option ->
  selection:Selection.t ->
  Obuilder_spec.t

val spec_make :
  repo:string option ->
  base:string ->
  opam_files:string list ->
  compiler_commit:Current_git.Commit_id.t option ->
  selection:Selection.t ->
  targets:string list ->
  Obuilder_spec.t

val spec_script :
  repo:string option ->
  base:string ->
  opam_files:string list ->
  compiler_commit:Current_git.Commit_id.t option ->
  selection:Selection.t ->
  cmds:string list ->
  Obuilder_spec.t
