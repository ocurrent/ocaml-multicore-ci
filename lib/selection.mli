(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  commit : string;                    (** A commit in opam-repository to use. *)
  command : string option;            (** The build command to use (will default to dune build @install @runtest if not specified) *)
} [@@deriving yojson, ord]

val of_worker : Ocaml_multicore_ci_api.Worker.Selection.t -> t

val remove_package : t -> package:string -> t
