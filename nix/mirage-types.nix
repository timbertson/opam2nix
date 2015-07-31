{stdenv, fetchurl, ocaml, findlib, cstruct, ocaml_ipaddr, ocaml_lwt, io-page, cmdliner }:
stdenv.mkDerivation {
	name = "mirage-types";
	src = fetchurl {
		url = "https://github.com/mirage/mirage/archive/v2.3.0.tar.gz";
		sha256 = "029kp4k0cdm0hxw3c0q5jngfi61ssppw6ydb2hb7gdr7gxhxx0qg";
	};
	configurePhase="true";
	buildPhase = "make types";
	installTargets = "install-types";
	buildInputs = [ ocaml findlib cstruct io-page ocaml_ipaddr ocaml_lwt cmdliner ];
	createFindlibDestdir = true;
}

