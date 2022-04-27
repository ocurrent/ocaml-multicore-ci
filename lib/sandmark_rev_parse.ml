open Lwt.Infix
open Current.Syntax

module Git = Current_git

let ( >>!= ) = Lwt_result.bind


module Op = struct
  type t = No_context
  let id = "sandmark-rev-parse"

  module Key = struct
    type t = {
      repo : string;  (* Remote repository from which to pull. *)
      gref : string;
    } [@@deriving to_yojson]

    let pp f t = Yojson.Safe.pretty_print f (to_yojson t)

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = struct
    type t = Git.Commit.t
    let digest = Git.Commit.hash
  end

  module Outcome = struct
    type t = {
      repo : string;
      gref : string;
      hash : string
    } [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e

  end

  let publish No_context job (key: Key.t) (commit: Value.t) =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Git.with_checkout ~job commit @@ fun path ->
      let cmd = ["git"; "-C"; Fpath.to_string path; "tag"] in
      (Current.Process.check_output ~cancellable:true ~job ("", Array.of_list cmd) >|= Stdlib.Result.map String.trim) >>!= fun tags ->
      let tags = String.split_on_char '\n' tags in
      let tags = List.map (fun x -> String.trim x) tags in
      let tag = List.find_map (
        fun x ->
          if String.equal x key.gref
            || String.equal x (String.cat "v" key.gref)
            || String.equal x (String.cat "V" key.gref)
            || String.equal x (String.cat "release-" key.gref) then Some x else None) tags in
      let gref = match tag with Some t -> t | None -> key.gref in
      let cmd = ["git"; "-C"; Fpath.to_string path; "rev-parse"; gref] in
      (Current.Process.check_output ~cancellable:true ~job ("", Array.of_list cmd) >|= Stdlib.Result.map String.trim) >>!= fun hash ->
      Lwt.return (Ok {Outcome.repo = key.repo; gref =  gref; hash = hash})

  let pp f (key, _) =
    Fmt.pf f "The rev-parse of %s in %s" key.Key.gref key.repo
  let auto_cancel = true
end

module Rev_parse_cache = Current_cache.Output(Op)

let v ~schedule ~gref ~repo ~commit =
  Current.component "rev-parse@ %s@ %s" gref repo |>
  let> commit = commit in
  Rev_parse_cache.set ~schedule Op.No_context { Op.Key.repo; gref } commit
