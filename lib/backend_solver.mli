open Capnp_rpc_lwt

type t

val make : Ocaml_multicore_ci_api.Solver.X.t Sturdy_ref.t -> t

val local : ?solver_dir:string -> unit -> t

val ci : t -> Ocaml_multicore_ci_api.Solver.t Lwt.t

