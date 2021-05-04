all: nix/opam-selection.nix
	nix-build -A hello

nix/opam-selection.nix: Makefile nix/default.nix default.nix
	nix-shell -A resolve default.nix
