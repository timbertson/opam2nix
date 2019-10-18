all: nix/opam-selection.nix
	nix-build default.nix

nix/opam-selection.nix: Makefile
	"`nix-build ../opam2nix.nix --no-out-link`/bin/opam2nix" resolve --dest $@ --ocaml-version 4.08.1 hello.opam
