# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
	opam-selection = opam2nix.build {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_08) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
in
opam-selection.hello
