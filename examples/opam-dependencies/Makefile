all: opam-selection.nix
	nix-build default.nix

opam-selection.nix: Makefile
	"`nix-build ../opam2nix.nix --no-out-link`/bin/opam2nix" resolve --ocaml-version 4.08.1 dune lwt
