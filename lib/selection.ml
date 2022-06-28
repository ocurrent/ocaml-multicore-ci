(** A set of packages for a single build. *)
type t = {
  variant : Variant.t;                (** The variant image to build on. *)
  packages : string list;             (** The selected packages ("name.version"). *)
  commits : (string * string) list;   (** The list of (repo_url,commit) opam-repositories from the analysis, and they're all usefull during the build.*)
  command : string option;            (** The build command to use (will default to dune build @install @runtest if not specified) *)
} [@@deriving yojson, ord]

let of_worker w =
  let module W = Ocaml_multicore_ci_api.Worker.Selection in
  let { W.id; packages; commits } = w in
  let variant = Variant.of_string id in
  let commits = commits in
  { variant; packages; commits; command=None }

let remove_package t ~package =
  {
    t with
    packages =
      List.filter (fun p -> not (String.equal p package)) t.packages;
  }
