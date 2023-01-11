val v :
  repo_url:string ->
  ?compiler_commit:Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t
(** Record a build starting for the given repo / commit. *)
