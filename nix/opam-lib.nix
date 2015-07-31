{stdenv, fetchurl, ocaml, findlib, ocaml_extlib, cmdliner, cudf, curl, dose, jsonm, ocamlgraph, re }:
stdenv.mkDerivation {
	name = "opam-lib";
	src = fetchurl {
		url = "https://github.com/ocaml/opam/archive/1.2.2.tar.gz";
		sha256 = "0sp3f54f27pfzy27gz72nsn37f9fj2z5x24x06xfrppqdzghajiy";
	};
	buildInputs = [ocaml findlib ocaml_extlib cmdliner cudf curl dose jsonm ocamlgraph re ];
	installPhase = "make libinstall";
	createFindlibDestdir = true;
}

