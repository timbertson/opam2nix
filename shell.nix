{ pkgs ? import <nixpkgs> {} }:
pkgs.lib.overrideDerivation (import nix/default.nix { inherit pkgs; }) (base: {
	buildInputs = base.buildInputs ++ base.devInputs;
})
