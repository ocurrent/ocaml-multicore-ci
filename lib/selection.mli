type t = {
  variant : Variant.t;  (** The variant image to build on. *)
  packages : string list;  (** The selected packages ("name.version"). *)
  commits : (string * string) list;
      (** The list of (repo_url,commit) opam-repositories from the analysis, and
          they're all usefull during the build.*)
  command : string option;
      (** The build command to use (will default to dune build @install @runtest if not specified) *)
}
[@@deriving yojson, ord]
(** A set of packages for a single build. *)

val of_worker : Ocaml_multicore_ci_api.Worker.Selection.t -> t
val remove_package : t -> package:string -> t
