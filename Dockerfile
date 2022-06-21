FROM ocaml/opam:debian-11-ocaml-4.14@sha256:70bd0da74f68550d667cc1a8ae80661e694cd9ac614273b23c45f3fb8a9f3ac9 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev libffi-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev graphviz -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard edb83a8bd94d816d2957fe8ee59f4617d67529fd && opam update
COPY --chown=opam \
	ocurrent/current_docker.opam \
	ocurrent/current_github.opam \
	ocurrent/current_git.opam \
	ocurrent/current.opam \
	ocurrent/current_rpc.opam \
	ocurrent/current_slack.opam \
	ocurrent/current_web.opam \
	/src/ocurrent/
COPY --chown=opam \
	ocluster/ocluster-api.opam \
	ocluster/current_ocluster.opam \
	/src/ocluster/
COPY --chown=opam \
	ocaml-version/ocaml-version.opam \
	/src/ocaml-version/
COPY --chown=opam \
	ocaml-dockerfile/dockerfile*.opam \
	/src/ocaml-dockerfile/
WORKDIR /src
RUN opam pin add -yn current_docker.dev "./ocurrent" && \
    opam pin add -yn current_github.dev "./ocurrent" && \
    opam pin add -yn current_git.dev "./ocurrent" && \
    opam pin add -yn current.dev "./ocurrent" && \
    opam pin add -yn current_rpc.dev "./ocurrent" && \
    opam pin add -yn current_slack.dev "./ocurrent" && \
    opam pin add -yn current_web.dev "./ocurrent" && \
    opam pin add -yn current_ocluster.dev "./ocluster" && \
    opam pin add -yn ocaml-version.dev "./ocaml-version" && \
    opam pin add -yn dockerfile.dev "./ocaml-dockerfile" && \
    opam pin add -yn dockerfile-opam.dev "./ocaml-dockerfile" && \
    opam pin add -yn ocluster-api.dev "./ocluster"
COPY --chown=opam ocaml-multicore-ci-service.opam ocaml-multicore-ci-api.opam ocaml-multicore-ci-solver.opam /src/
RUN opam-2.1 install -y --deps-only .
ADD --chown=opam . .
RUN opam-2.1 config exec -- dune build ./_build/install/default/bin/ocaml-multicore-ci-service

FROM debian:11
RUN apt-get update && apt-get install libev4 openssh-client curl gnupg2 dumb-init git graphviz libsqlite3-dev ca-certificates netbase -y --no-install-recommends
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN echo 'deb [arch=amd64] https://download.docker.com/linux/debian buster stable' >> /etc/apt/sources.list
RUN apt-get update && apt-get install docker-ce -y --no-install-recommends
WORKDIR /var/lib/ocurrent
ENTRYPOINT ["dumb-init", "/usr/local/bin/ocaml-multicore-ci-service"]
ENV OCAMLRUNPARAM=a=2
# Enable experimental for docker manifest support
ENV DOCKER_CLI_EXPERIMENTAL=enabled
COPY --from=build /src/_build/install/default/bin/ocaml-multicore-ci-service /src/_build/install/default/bin/ocaml-multicore-ci-solver /usr/local/bin/
