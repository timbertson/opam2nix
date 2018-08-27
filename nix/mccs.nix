{stdenv, fetchurl, ocaml, dune, findlib, cudf, ocaml_extlib }:
stdenv.mkDerivation {
	name = "mccs";
	src = fetchurl {
		url = "https://github.com/AltGr/ocaml-mccs/archive/1.1+8.tar.gz";
		sha256 = "0xavfvxfrcf3lmry8ymma1yzy0hw3ijbx94c9zq3pzlwnylrapa4";
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

