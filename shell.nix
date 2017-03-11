{ pkgs ? import <nixpkgs> {} }:
pkgs.lib.overrideDerivation (import nix/local.nix { inherit pkgs; }) (base: {
	buildInputs = base.buildInputs ++ base.devInputs;
})
