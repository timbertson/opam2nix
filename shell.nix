{ pkgs ? import <nixpkgs> {} }:
let base = (pkgs.callPackage nix/default.nix {}); in
base.overrideAttrs (attrs: {
	buildInputs = attrs.buildInputs ++ base.devInputs;
})
