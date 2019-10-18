# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, fetchFromGitHub, opam2nix }:
let
	vdoml = fetchFromGitHub {
		owner = "timbertson";
		repo = "vdoml";
		rev = "f3b9e7b55e0aef12bb6f848f83aa7bfca5c29b7c";
		sha256 = "1gjl98ly1ldxni6iswkl50i62bwriz0kpn12s03lbsrpaf54kibc";
	};
	opam-selection = opam2nix.build {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_08) ocaml;
		selection = ./opam-selection.nix;
		src = {
			inherit vdoml;
			hello = ../.;
		};
	};
in
{
	inherit (opam-selection) hello;
	inherit vdoml;
	inherit opam2nix;
}

