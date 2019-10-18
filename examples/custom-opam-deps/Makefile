all: nix/opam-selection.nix
	nix-build default.nix -A hello

OPAM2NIX = "$(shell nix-build -A opam2nix --no-out-link)/bin/opam2nix"
VDOML = "$(shell nix-build -A vdoml --no-out-link)"

nix/opam-selection.nix: Makefile
	$(OPAM2NIX) resolve --dest $@ --ocaml-version 4.08.1 $(VDOML)/vdoml.opam hello.opam
