{stdenv, fetchurl, ocaml, findlib, camlp4, ocaml_extlib, cudf, ocamlgraph, cppo, re, perl }:
stdenv.mkDerivation {
	name = "dose";
	src = fetchurl {
		url = "http://gforge.inria.fr/frs/download.php/file/36063/dose3-5.0.1.tar.gz";
		sha256 = "00yvyfm4j423zqndvgc1ycnmiffaa2l9ab40cyg23pf51qmzk2jm";
	};
	buildInputs = [ocaml findlib camlp4 ocaml_extlib cudf ocamlgraph cppo re perl ];
	# configureFlags = "--with-ocamlgraph";
	createFindlibDestdir = true;
	patchPhase = ''
		sed -i 's@$(LIBDIR)@$(OCAMLFIND_DESTDIR)@' Makefile.config.in
	'';
	patches = [
		# ./dose/0003-Removed-hard-failure-cases-in-favor-of-finer-diagnos.patch
		# ./dose/0004-Remove-broken-assert.patch
		# ./dose/0005-Add-a-check_request-function-allowing-more-control-o.patch

		# ./dose/0002-ocamlgraph-1.8.6.diff {ocamlgraph:version >= "1.8.6"}
	];
}

