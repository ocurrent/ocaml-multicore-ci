let src =
  Logs.Src.create "ocaml_multicore_ci_web"
    ~doc:"ocaml-multicore-ci web interface"

include (val Logs.src_log src : Logs.LOG)
