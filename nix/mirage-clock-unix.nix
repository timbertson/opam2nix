{stdenv, fetchurl, ocaml, findlib, mirage-types }:
stdenv.mkDerivation {
	name = "mirage-clock-unix";
	src = fetchurl {
		url = "https://github.com/mirage/mirage-clock/archive/v1.0.0.tar.gz";
		sha256 = "1lvgp9wvamrd3h5ql4rj61ypjq9xjmmnanqd6v5gv2l9398skhid";
	};
	buildInputs = [ ocaml findlib mirage-types ];
	buildPhase = "make unix-build";
	installTargets = "unix-install";
	createFindlibDestdir = true;
}
