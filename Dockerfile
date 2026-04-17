# syntax=docker/dockerfile:1.7

ARG OCAML_SWITCH=5.1.1

FROM ubuntu:24.04 AS builder

ARG OCAML_SWITCH
ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-lc"]

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        opam \
        m4 \
        pkg-config \
        libgmp-dev \
        clang \
        build-essential \
        ca-certificates \
        git \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/forge

RUN opam init -a --disable-sandboxing --bare \
    && opam switch create "${OCAML_SWITCH}" \
    && eval "$(opam env --switch="${OCAML_SWITCH}")" \
    && opam install -y dune

COPY . .

RUN eval "$(opam env --switch="${OCAML_SWITCH}")" \
    && dune build \
        bin/interpreter.exe \
        bin/repl.exe \
        bin/compile_forge.exe

FROM ubuntu:24.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        clang \
        libgmp10 \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

ENV FORGE_ROOT=/opt/forge
ENV DUNE_SOURCEROOT=/opt/forge

COPY --from=builder /opt/forge/prelude /opt/forge/prelude
COPY --from=builder /opt/forge/runtime /opt/forge/runtime
COPY --from=builder /opt/forge/programs /opt/forge/programs
COPY --from=builder /opt/forge/examples /opt/forge/examples

COPY --from=builder /opt/forge/_build/default/bin/interpreter.exe /usr/local/bin/forge-interpreter
COPY --from=builder /opt/forge/_build/default/bin/repl.exe /usr/local/bin/forge-repl
COPY --from=builder /opt/forge/_build/default/bin/compile_forge.exe /usr/local/bin/forge-compile
COPY docker/entrypoint.sh /usr/local/bin/forge

RUN chmod +x \
      /usr/local/bin/forge \
      /usr/local/bin/forge-interpreter \
      /usr/local/bin/forge-repl \
      /usr/local/bin/forge-compile

ENTRYPOINT ["/usr/local/bin/forge"]
CMD ["help"]
