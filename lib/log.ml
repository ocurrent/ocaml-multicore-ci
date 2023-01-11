let src =
  Logs.Src.create "ocaml_multicore_ci_solver"
    ~doc:"ocaml-multicore-ci solver backend "

include (val Logs.src_log src : Logs.LOG)
