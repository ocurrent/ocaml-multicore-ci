# Bonsai'd Multicore

POC for building a JSOO/Bonsai UI for Multicore pipeline.

TODO
 * promote main.bc.js to top level
 * serve static assets 
 * Setup server side to provide Graphql


Run backend server

``` shell
dune exec -- ocaml-multicore-ci-service --github-app-id 123448  \
  --github-private-key-file ~/projects/tarides/github-secret-key.pem  \
  --github-account-allowlist tmcgilchrist \
  --submission-service ~/projects/tarides/ocurrent-submission.cap   \
  --github-webhook-secret-file ~/projects/tarides/github-app-secret \
  --capnp-address tcp:127.0.0.1:9000
```

Run web server
``` shell
dune exec -- ocaml-multicore-ci-web --backend ./capnp-secrets/ocaml-ci-admin.cap
```

