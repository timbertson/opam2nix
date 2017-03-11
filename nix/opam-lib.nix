{stdenv, fetchurl, ocaml, findlib, ocaml_extlib, cmdliner, cudf, curl, dose3, jsonm, ocamlgraph, re }:
stdenv.mkDerivation {
	name = "opam-lib";
	src = fetchurl {
		url = "http://github.com/ocaml/opam/archive/1.3.0.tar.gz";
		sha256 = "0yhgy6ap6bsj1gclgazjn98sizja9qmb2fgfvk102i7l80li4l4x";
	};
	buildInputs = [ocaml findlib ocaml_extlib cmdliner cudf curl dose3 jsonm ocamlgraph re ];
	installPhase = "make libinstall";
	createFindlibDestdir = true;
}

