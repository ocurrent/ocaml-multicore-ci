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
    ?is_compiler:bool ->
    ?compiler_commit:Current_git.Commit_id.t ->
    Fpath.t ->
    (t, [ `Msg of string ]) result Lwt.t
end

val examine :
  ?label:string ->
  solver:Ocaml_multicore_ci_api.Solver.t ->
  platforms:Platform.t list Current.t ->
  opam_repository_commits:Current_git.Commit_id.t list Current.t ->
  is_compiler:bool ->
  get_is_compiler_blocklisted:(Ocaml_version.t -> string -> bool) ->
  repo:string Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** [examine ~solver ~platforms ~opam_repository_commit src] analyses the source code [src] and selects
    package versions to test using [opam_repository_commit]. *)

val examine_with_compiler :
  ?label:string ->
  solver:Ocaml_multicore_ci_api.Solver.t ->
  platforms:Platform.t list Current.t ->
  opam_repository_commits:Current_git.Commit_id.t list Current.t ->
  compiler_commit:Current_git.Commit_id.t Current.t ->
  Current_git.Commit.t Current.t ->
  Analysis.t Current.t
(** Similar to [examine] except with the differing assumption that we will
    be compiling using the specified compiler_commit. This means that we
    won't build under all platforms, we'll just pick one to suit the
    compiler. It also means that we're not going to pin local opam files,
    but we'll build from the published opam-repository instead. *)
