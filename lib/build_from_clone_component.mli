(** Record a build starting for the given repo / commit. *)
val v :
  repo_url:string ->
  ?compiler_commit:Current_git.Commit.t Current.t ->
  ?dependency:unit Current.t ->
  Current_git.Commit.t Current.t ->
  Current_git.Commit.t Current.t
