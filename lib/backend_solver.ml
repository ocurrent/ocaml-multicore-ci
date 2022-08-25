open Capnp_rpc_lwt
open Lwt.Infix

let retry_delay = 5.0   (* Time to wait after a failed connection before retrying. *)

module Metrics = struct
  open Prometheus

  let namespace = "ocaml_multicore_ci"
  let subsystem = "solver"

  let backend_down =
    let help = "Whether the connection to the Solver is down" in
    Gauge.v ~help ~namespace ~subsystem "sovler_down"
end

type conn_remote = {
  sr : Ocaml_multicore_ci_api.Solver.X.t Sturdy_ref.t;
  mutable ci : Ocaml_multicore_ci_api.Solver.t Lwt.t;
  mutable last_failed : float;
}

type t = [`Remote of conn_remote | `Local of Ocaml_multicore_ci_api.Solver.t Lwt.t]

let rec reconnect t =
  Prometheus.Gauge.set Metrics.backend_down 1.0;
  let now = Unix.gettimeofday () in
  let delay = t.last_failed +. retry_delay -. now in
  t.last_failed <- now;
  Lwt.async (fun () ->
      begin if delay > 0.0 then Lwt_unix.sleep delay else Lwt.return_unit end
      >>= fun () ->
      Log.info (fun f -> f "Reconnecting to the Solver");
      let ci =
        try Sturdy_ref.connect_exn t.sr
        with ex -> Lwt.fail ex    (* (just in case) *)
      in
      t.ci <- ci;
      monitor t;
      Lwt.return_unit
    )
and monitor t =
  Lwt.on_any t.ci
    (fun ci ->
       (* Connected OK - now watch for failure. *)
       Prometheus.Gauge.set Metrics.backend_down 0.0;
       ci |> Capability.when_broken @@ fun ex ->
       Log.warn (fun f -> f "Lost connection to the Solver: %a" Capnp_rpc.Exception.pp ex);
       reconnect t
    )
    (fun ex ->
       (* Failed to connect. *)
       Log.warn (fun f -> f "Failed to connect to the Solver: %a" Fmt.exn ex);
       reconnect t
    )

let make sr : t =
    let ci = Sturdy_ref.connect_exn sr in
    let t = { sr; ci; last_failed = 0.0 } in
    Prometheus.Gauge.set Metrics.backend_down 1.0;
    monitor t;
    `Remote t

let local ?solver_dir () =
  `Local (Lwt.return (Solver_pool.spawn_local ?solver_dir ()))

let ci t =
  match t with
  | `Remote t -> t.ci
  | `Local t  -> t
