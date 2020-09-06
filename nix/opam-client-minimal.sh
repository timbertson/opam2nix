#!/usr/bin/env bash
# Script to hack up opam-client to remove dependency on
# opam-state and opam-solver, since we only require one
# function from it (prepare_package_build)

set -eu

function sed_change {
	filename="$1"
	shift
	initial_contents="$(cat "$filename")"
	# cat "$filename" | sed -e 's/^/-- /'
	echo "+ sed -i $*"
	sed -i "$@" "$filename"
	if diff -q "$filename" <(echo "$initial_contents") >/dev/null; then
		# cat "$filename"
		echo "Error: $filename unchanged after: sed $*"
		exit 1
	fi
}

function patch_ml {
	echo "== Patching $1"
	contents="$(cat "$1")"
	# grab imports
	echo "$contents" | sed -e '/module PackageActionGraph/,$ d' > "$1"
	echo "Imports: $(wc -l "$1") lines"
	# and prepare_package_build function
	echo "$contents" | sed -n -e '/^let prepare_package_build/,/^let/p' >> "$1"
	echo "Add prepare_package_build: $(wc -l "$1") lines"

	# Drop the `let` from above
	sed_change "$1" -e '$d'
	sed_change "$1" -e '/open OpamStateTypes/d'

	# Short circuit checks that require extra modules
	sed_change "$1" -e 's/OpamClientConfig.(!r.fake)/false/'
	sed_change "$1" -e 's/OpamStateConfig.(!r.dryrun)/false/'
}

function patch_dune {
	echo "== Patching $1"
	contents="$(cat "$1")"
	# build only OpamAction
	sed_change "$1" -e 's/(modules .*/(modules opamAction)/'
	# Drop opam-solver, opam-state and replace with opam-format, opam-core
	sed_change "$1" -e 's/opam-solver//'
	sed_change "$1" -e 's/opam-state//'
	sed_change "$1" -e 's/opam-repository//'
	sed_change "$1" -e 's/(libraries /(libraries opam-format opam-core/'
	# only build the library
	sed_change "$1" -e '/^(executable/,$d'
}

patch_dune src/client/dune
patch_ml src/client/opamAction.ml
rm -f src/client/opamAction.mli
