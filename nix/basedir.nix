{stdenv, fetchurl, ocaml, ocamlbuild, findlib, fileutils, ounit, ocaml_oasis}:

stdenv.mkDerivation {
  name = "ocaml-basedir";

  src = fetchurl {
    url = "https://github.com/gildor478/ocaml-xdg-basedir/archive/0.0.3.tar.gz";
    sha256 = "0flk9p9yvyypqvh43d77xdmw6bm90qjrih02x2hipaynsw81irf6";
  };

  buildInputs = [ ocaml ocamlbuild findlib ounit ocaml_oasis ];
  OCAML_TOPLEVEL_PATH = "${findlib}/lib/ocaml/${ocaml.version}/site-lib";
  propagatedBuildInputs = [ fileutils ];
  createFindlibDestdir = true;
}
