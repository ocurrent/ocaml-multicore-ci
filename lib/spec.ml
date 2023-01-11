type opam_files = string list [@@deriving to_yojson, ord]

type ty =
  [ `Opam of
    [ `Build
    | `Lint of [ `Doc | `Opam ]
    | `Make of string list
    | `Script of string list ]
    * Selection.t
    * opam_files
  | `Opam_fmt of Analyse_ocamlformat.source option
  | `Opam_monorepo of Opam_monorepo.config ]
[@@deriving to_yojson, ord]

type t = { label : string; variant : Variant.t; ty : ty } [@@deriving ord]

let opam ~label ~selection ~analysis op =
  let variant = selection.Selection.variant in
  let ty =
    match op with
    | (`Build | `Lint (`Doc | `Opam) | `Make _ | `Script _) as x ->
        `Opam (x, selection, Analyse.Analysis.opam_files analysis)
    | `Lint `Fmt -> `Opam_fmt (Analyse.Analysis.ocamlformat_source analysis)
  in
  { label; variant; ty }

let opam_monorepo ~config =
  let { Selection.variant; _ } = Opam_monorepo.selection_of_config config in
  { label = Variant.to_string variant; variant; ty = `Opam_monorepo config }

let pp f t = Fmt.string f t.label
let label t = t.label

let pp_summary f = function
  | `Opam (`Build, _, _) -> Fmt.string f "Opam project build using dune"
  | `Opam (`Lint `Doc, _, _) -> Fmt.string f "Opam project lint documentation"
  | `Opam (`Lint `Opam, _, _) -> Fmt.string f "Opam files lint"
  | `Opam (`Make _, _, _) -> Fmt.string f "Opam project build using make"
  | `Opam (`Script _, _, _) ->
      Fmt.string f "Opam project build using an arbitrary script"
  | `Opam_fmt v ->
      Fmt.pf f "ocamlformat version: %a"
        Fmt.(option ~none:(any "none") Analyse_ocamlformat.pp_source)
        v
  | `Opam_monorepo _ -> Fmt.string f "opam-monorepo build"

let digest_of_ty ~variant ty =
  let hash_packages packages =
    Digest.string (String.concat "," packages) |> Digest.to_hex
  in
  match ty with
  | `Opam (`Build, selection, _) -> hash_packages selection.Selection.packages
  | `Opam (`Lint (`Doc | `Opam), selection, _) ->
      hash_packages selection.packages
  | `Opam (`Make targets, selection, _) ->
      hash_packages (selection.packages @ targets)
  | `Opam (`Script cmds, selection, _) ->
      hash_packages (selection.packages @ cmds)
  | `Opam_fmt _ -> "ocamlformat"
  | `Opam_monorepo _ -> "opam-monorepo-" ^ Variant.to_string variant

let success_result_of_ty = function
  | `Opam_monorepo _
  | `Opam (`Build, _, _)
  | `Opam (`Make _, _, _)
  | `Opam (`Script _, _, _) ->
      `Built
  | `Opam (`Lint (`Doc | `Opam), _, _) | `Opam_fmt _ -> `Checked

let success_result spec = success_result_of_ty spec.ty
