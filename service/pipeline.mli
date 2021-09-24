val local_test : ?label: string -> solver:Ocaml_multicore_ci_api.Solver.t -> Current_git.Local.t -> unit -> unit Current.t
(** [local_test ~solver repo] is a pipeline that tests local repository [repo] as the CI would. *)

val local_test_multiple : solver:Ocaml_multicore_ci_api.Solver.t -> Current_git.Local.t list -> unit -> unit Current.t
(** [local_test_multiple ~solver repos] runs multiple local repository tests in parallel. *)

val local_test_fixed : solver:Ocaml_multicore_ci_api.Solver.t -> unit -> unit Current.t
(** [local_test_fixed ~solver] runs local builds on the fixed set of configured repositories. *)

val v :
  ?ocluster:Cluster_api.Raw.Client.Submission.t Capnp_rpc_lwt.Sturdy_ref.t ->
  solver:Ocaml_multicore_ci_api.Solver.t ->
  unit -> unit Current.t
(** The main ocaml-multicore-ci pipeline. Tests everything configured for GitHub application [app]. *)
