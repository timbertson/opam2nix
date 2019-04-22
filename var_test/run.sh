#!/usr/bin/env bash
set -eu
cd "$(dirname "$0")"
env \
	out='$out' \
	OCAMLRUNPARAM=b \
	opamEnv="$(cat env.json | tr -d "\n")" \
	../bin/opam2nix invoke dump
