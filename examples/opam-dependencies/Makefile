all: opam-packages.nix
	nix-build default.nix

opam-packages.nix: Makefile
	"`nix-build ../opam2nix.nix --no-out-link`/bin/opam2nix" update --ocaml-version 4.08.1 ocamlbuild ocamlfind lwt
