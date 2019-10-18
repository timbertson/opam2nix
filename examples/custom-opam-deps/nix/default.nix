{ pkgs, stdenv, opam2nix }:
opam2nix.buildOpamPackage rec {
	version = "0.0.1";
	name = "hello-${version}";
	src = ../.;
	# verbose = true;

	# opam specific settings:
	# ocamlAttr = "ocaml_4_03";
	# packageName = "hello"; # default: derived from `name` attribute
	# opamFile = "hello.opam"; # default: "${packageName}.opam"
}
