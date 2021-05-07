module Analysis : sig
  type t [@@deriving yojson]

  val opam_files : t -> string list
  val ocamlformat_source : t -> Analyse_ocamlformat.source option

  val selections : t -> [
      | `Opam_build of Selection.t list
      | `Opam_monorepo of Opam_monorepo.config
      | `Not_opam of string * Selection.t list
    ]

  val of_dir :
    solver:Ocaml_multicore_ci_api.Solver.t ->
    job:Current.Job.t ->
    platforms:(Variant.t * Ocaml_multicore_ci_api.Worker.Vars.t) list ->
    opam_repository_commits:Current_git.Commit_id.t list ->
    package_name:string ->
    Fpath.t ->
    (t, [ `Msg of string ]) result Lwt.t
end

val examine :
  ?label:string ->
  solver:Ocaml_multicore_ci_api.Solver.t ->
  platforms:Platform.t list Current.t ->
  opam_repository_commits:Current_git.Commit_id.t list Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** [examine ~solver ~platforms ~opam_repository_commit src] analyses the source code [src] and selects
    package versions to test using [opam_repository_commit]. *)
