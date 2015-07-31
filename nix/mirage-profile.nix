{stdenv, fetchurl, ocaml, findlib, cstruct, ocaml_lwt, io-page, ocplib-endian }:
stdenv.mkDerivation {
	name = "mirage-profile";
	src = fetchurl {
		url = "https://github.com/mirage/mirage-profile/archive/v0.5.tar.gz";
		sha256 = "11ccgc9lv6d25vh50gfx693q3h9n41akdvqswr6dvvglag4nv2mf";
	};
	buildInputs = [ ocaml findlib cstruct io-page ocaml_lwt ocplib-endian ];
	propagateBuildInputs = [ cstruct io-page ocaml_lwt ocplib-endian ];
	createFindlibDestdir = true;
}

