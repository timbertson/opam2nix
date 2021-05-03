{ pkgs ? import <nixpkgs> {}, opam2nix ? import ../opam2nix.nix, name ? null, version ? null, resolveArgs ? [] }:
with pkgs;
let
	opam-args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_10) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
in
{
	resolve = opam2nix.resolve opam-args ([
		(if version == null then name else "${name}=${version}")
	] ++ resolveArgs);
	selection = opam2nix.build opam-args;
}
