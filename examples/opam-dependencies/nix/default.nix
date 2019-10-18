# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
stdenv.mkDerivation {
	name = "opam2nix-hello";
	src = ../.;
	buildInputs = opam2nix.buildInputs {
		ocaml = pkgs.ocaml-ng.ocamlPackages_4_08.ocaml;
		selection = ../opam-selection.nix;
	};
	buildPhase = ''
		ocamlbuild -use-ocamlfind hello.native
	'';
	installPhase = ''
		mkdir $out/bin
		cp --dereference hello.native $out/bin/hello
	'';
}
