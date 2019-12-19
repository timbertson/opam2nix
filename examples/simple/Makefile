all: nix/opam-selection.nix
	nix-build -A hello

nix/opam-selection.nix: Makefile
	nix-shell -A resolve
