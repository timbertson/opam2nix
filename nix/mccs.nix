{ocamlPackages, stdenv, fetchurl, ocaml, dune, findlib, cudf, ocaml_extlib }:
ocamlPackages.buildDunePackage rec {
	pname = "mccs";
	version = "1.1+9";
	src = fetchurl {
		url = "https://github.com/AltGr/ocaml-mccs/archive/${version}.tar.gz";
		sha256 = "0gf86c65jdxxcwd96kcmrqxrmnnzc0570gb9ad6c57rl3fyy8yhv";
	};
	buildInputs = [ findlib ocaml dune cudf ocaml_extlib ];
	configurePhase = "true";
}

