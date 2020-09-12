#!/usr/bin/env bash

set -eux
name="$1"
if [ "$#" -gt 1 ]; then
	version="\"$2\""
else
	version="null"
fi
nix-shell --arg version "$version" --argstr name "$name" generic.nix -A resolve --arg resolveArgs "${RESOLVE_ARGS:-[]}"
nix-build generic.nix -A "selection.$name" --show-trace
