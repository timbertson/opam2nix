{stdenv, fetchurl, ocaml, dune, findlib, cudf, ocaml_extlib }:
stdenv.mkDerivation {
	name = "mccs";
	src = fetchurl {
		url = "https://github.com/AltGr/ocaml-mccs/archive/1.1+9.tar.gz";
		sha256 = "0gf86c65jdxxcwd96kcmrqxrmnnzc0570gb9ad6c57rl3fyy8yhv";
	};
	buildInputs = [ findlib ocaml dune cudf ocaml_extlib ];
	configurePhase = "true";
	buildPhase = ''
		dune build -p mccs
	'';
	installPhase = ''
		mkdir -p $out
		dune install --prefix $out
	'';
	createFindlibDestdir = true;
}

