#!/usr/bin/env bash
set -eux
cd "$(dirname "$0")"
env \
	OPAM2NIX_DEBUG=true \
	OCAMLRUNPARAM=b \
	opamEnv="$(cat opamEnv | tr -d "\n")" \
	../bin/opam2nix invoke dump
