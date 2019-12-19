# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
	args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_08) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
	opam-selection = opam2nix.build args;
	resolve = opam2nix.resolve args [ "hello.opam" ];
in
{
	inherit (opam-selection) hello;
	inherit resolve;
}
