#!/bin/bash
set -eu
gup="$(nix-build --show-trace --no-out-link ./nix/gup.nix)"
[ -n "$gup" ] || exit 1
"$gup/bin/gup" local
