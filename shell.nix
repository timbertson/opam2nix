{ pkgs ? import <nixpkgs> {} }:
pkgs.lib.overrideDerivation (pkgs.callPackage nix/default.nix {}) (base: {
	buildInputs = base.buildInputs ++ base.devInputs;
})
