open Lwt.Infix
open Current.Syntax
module Git = Current_git

module Op = struct
  type t = No_context

  let id = "build_from_clone_component"

  module Key = struct
    type t = {
      repo_url : string;
      commit : Git.Commit.t;
      compiler_commit : Git.Commit.t option;
    }

    let digest t =
      Fmt.str "%s / %a / %a" t.repo_url Git.Commit.pp t.commit
        (Fmt.option Git.Commit.pp) t.compiler_commit
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

  let publish No_context job (key : Key.t) (commit : Value.t) =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    let repo_id = Repo_url_utils.repo_id_from_url key.repo_url in
    let commit_ids = [ Current_git.Commit.id commit ] in
    Pipeline_utils.set_active_refs_by_id ~repo_id commit_ids;
    Lwt.return (Ok commit)

  let pp f (key, commit) =
    let with_compiler_str =
      match key.Key.compiler_commit with
      | None -> "with compiler from Docker image"
      | Some cc ->
          Fmt.str "with compiler %a" Git.Commit_id.pp (Git.Commit.id cc)
    in
    Fmt.pf f "Building %s from %a %s" key.Key.repo_url Git.Commit.pp commit
      with_compiler_str

  let auto_cancel = true
end

module BC = Current_cache.Output (Op)

let v ~repo_url ?compiler_commit commit =
  let label = Repo_url_utils.owner_slash_name_from_url repo_url in
  let cc = match compiler_commit with None -> "" | Some _ -> "(c)" in
  Current.component "build@ %s@ %s" label cc
  |> let> commit = commit
     and> compiler_commit = Current.option_seq compiler_commit in
     BC.set No_context { repo_url; commit; compiler_commit } commit
