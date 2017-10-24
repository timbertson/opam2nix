{stdenv, fetchurl, ocaml, findlib, ocaml_extlib, cmdliner, cudf, curl, dose3, jsonm, ocamlgraph, re }:
stdenv.mkDerivation {
	name = "opam-lib-1.3.1";
	src = fetchurl {
		url = "http://github.com/ocaml/opam/archive/1.3.1.tar.gz";
		sha256 = "1zg92zzzp4nvrw0170mhdml336r7fjf49ldak63wq6aj8v1nfmv2";
	};

	# https://github.com/ocaml/opam/pull/2897
	patches = [ ./opam-url-fix.diff ];

	buildInputs = [ocaml findlib ocaml_extlib cmdliner cudf curl dose3 jsonm ocamlgraph re ];
	installPhase = "make libinstall";
	createFindlibDestdir = true;
}

