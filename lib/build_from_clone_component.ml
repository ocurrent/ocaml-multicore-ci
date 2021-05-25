open Lwt.Infix
open Current.Syntax
module Git = Current_git

module Op = struct
  type t = No_context
  let id = "build_from_clone_component"

  module Key = struct
    type t = string  (* repo_url *)
    let digest t = t
  end

  module Value = struct
    type t = Git.Commit.t
    let digest = Git.Commit.hash
  end

  module Outcome = struct
    type t = Git.Commit.t
    let marshal = Git.Commit.marshal
    let unmarshal = Git.Commit.unmarshal
  end

  let publish No_context job (repo_url: Key.t) (commit: Value.t) =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    let repo_id = Repo_url_utils.repo_id_from_url repo_url in
    let commit_ids = [Current_git.Commit.id commit] in
    Pipeline_utils.set_active_refs_by_id ~repo_id commit_ids;
    Lwt.return (Ok commit)

  let pp f (repo_url, commit) =
    Fmt.pf f "Building %s from %a" repo_url Git.Commit.pp commit

  let auto_cancel = true
end

module BC = Current_cache.Output(Op)

let v ~repo_url commit =
  let label = Repo_url_utils.owner_slash_name_from_url repo_url in
  Current.component "build@ %s" label |>
  let> commit = commit in
  BC.set No_context repo_url commit
