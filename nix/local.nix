{ pkgs ? import <nixpkgs> {} }:
with pkgs;
lib.overrideDerivation (callPackage ./opam2nix.nix {}) (base: {
	src = ./local.tgz;
})
