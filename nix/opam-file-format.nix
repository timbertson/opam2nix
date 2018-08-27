{stdenv, fetchFromGitHub, ocaml, findlib }:
stdenv.mkDerivation {
	name = "opam-file-format";
	src = fetchFromGitHub {
		owner = "ocaml";
		repo = "opam-file-format";
		rev = "2.0.0";
		sha256 = "0fqb99asnair0043hhc8r158d6krv5nzvymd0xwycr5y72yrp0hv";
	};
	buildInputs = [ocaml findlib];
	createFindlibDestdir = true;
	installPhase = "make install PREFIX=$out LIBDIR=$out/lib/ocaml/${ocaml.version}/site-lib/";
}
