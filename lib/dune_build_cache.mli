val for_repo : string -> Obuilder_spec.Cache.t
(** Persist the [_build] directory between runs. Note that this different from
    the "dune cache" as described in
    https://dune.readthedocs.io/en/stable/caching.html. *)
