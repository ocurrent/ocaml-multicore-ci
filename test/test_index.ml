module Index = Ocaml_multicore_ci.Index
module Ref_map = Index.Ref_map

let jobs =
  let state f (variant, state) = Fmt.pf f "%s:%a" variant Index.pp_job_state state in
  Alcotest.testable (Fmt.Dump.list state) (=)

let test_simple () =
  let owner = "owner" in
  let name = "name" in
  let repo = { Current_github.Repo_id.owner; name } in
  let hash = "abc" in
  let db = Lazy.force Current.Db.v in
  Index.init ();
  Current.Db.exec_literal db "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, finished, build)
                                     VALUES ('test', x'00', 'job1', x'01', 1, x'02', '2019-11-01 9:00', '2019-11-01 9:01', '2019-11-01 9:02', 0)";
  Index.set_active_refs ~repo @@ Ref_map.singleton "master" hash;
  Index.record ~owner ~name ~hash ~status:`Pending ~variant:"analysis" (Some "job1");
  Index.record ~owner ~name ~hash ~status:`Pending ~variant:"alpine" None;
  Alcotest.(check (list (pair string string))) "Refs" ["master", hash] (Index.get_active_refs repo |> Ref_map.bindings);
  Alcotest.(check jobs) "Jobs A" ["alpine", `Not_started; "analysis", `Passed] @@ Index.get_jobs ~owner ~name hash;
  Current.Db.exec_literal db "INSERT INTO cache (op, key, job_id, value, ok, outcome, ready, running, finished, build)
                                     VALUES ('test', x'01', 'job2', x'01', 0, x'21', '2019-11-01 9:03', '2019-11-01 9:04', '2019-11-01 9:05', 0)";
  Index.record ~owner ~name ~hash ~status:`Failed ~variant:"analysis" (Some "job1");
  Index.record ~owner ~name ~hash ~status:`Failed ~variant:"alpine" (Some "job2");
  Alcotest.(check jobs) "Jobs B" ["alpine", `Failed "!"; "analysis", `Passed] @@ Index.get_jobs ~owner ~name hash;
  Index.remove ~owner ~name ~hash ~variant:"alpine";
  Index.record ~owner ~name ~hash ~status:`Passed ~variant:"analysis" (Some "job1");
  Alcotest.(check jobs) "Jobs C" ["analysis", `Passed] @@ Index.get_jobs ~owner ~name hash

let tests = [
    Alcotest_lwt.test_case_sync "simple" `Quick test_simple;
  ]
