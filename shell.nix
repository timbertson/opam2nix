{ pkgs ? import <nixpkgs> {} }:
let base = (pkgs.callPackage ./default.nix {}); in
base.overrideAttrs (attrs: {
	buildInputs = attrs.buildInputs ++ base.devInputs;
})
