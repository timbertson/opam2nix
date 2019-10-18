{ pkgs, stdenv, opam2nix }:
(opam2nix.buildOpamPackages [
	(rec {
		version = "0.0.1";
		name = "hello-${version}";
		src = ../.;
		# packageName = "hello"; # default: derived from `name` attribute
		# opamFile = "hello.opam"; # default: "${packageName}.opam"
	})
] {

	ocamlAttr = "ocaml_4_03";
}).packages.hello
