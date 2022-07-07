open Capnp_rpc_lwt

type t

val make : Ocaml_multicore_ci_api.Raw.Client.CI.t Sturdy_ref.t -> t

val ci : t -> Ocaml_multicore_ci_api.Client.CI.t Lwt.t
