{stdenv, fetchurl, ocaml, findlib, ocamlbuild, camlp4, ocaml_extlib, cudf, ocamlgraph, cppo, re, perl }:
stdenv.mkDerivation {
	name = "dose";
	src = fetchurl {
		url = "http://gforge.inria.fr/frs/download.php/file/36063/dose3-5.0.1.tar.gz";
		sha256 = "00yvyfm4j423zqndvgc1ycnmiffaa2l9ab40cyg23pf51qmzk2jm";
	};
	buildInputs = [ocaml findlib ocamlbuild camlp4 ocaml_extlib cudf ocamlgraph cppo re perl ];
	createFindlibDestdir = true;
	patches = [
		./dose/compilation.diff
	];
}

