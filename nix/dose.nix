{stdenv, fetchurl, ocaml, findlib, ocaml_extlib, cudf, ocamlgraph, re, perl }:
stdenv.mkDerivation {
	name = "dose";
	src = fetchurl {
		url = "https://gforge.inria.fr/frs/download.php/file/33677/dose3-3.2.2.tar.gz";
		sha256 = "174hzkggvjrqgzrzf9m6ilndzwsir50882fpjvgd33i9kygih2m3";
	};
	buildInputs = [ocaml findlib ocaml_extlib cudf ocamlgraph re perl ];
	configureFlags = "--with-ocamlgraph";
	createFindlibDestdir = true;
	patches = [
		./dose/0003-Removed-hard-failure-cases-in-favor-of-finer-diagnos.patch
		./dose/0004-Remove-broken-assert.patch
		./dose/0005-Add-a-check_request-function-allowing-more-control-o.patch
		# ./dose/0002-ocamlgraph-1.8.6.diff {ocamlgraph:version >= "1.8.6"}
	];
}

