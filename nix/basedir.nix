{stdenv, fetchurl, ocaml, findlib, fileutils, ounit}:

stdenv.mkDerivation {
  name = "ocaml-basedir";

  src = fetchurl {
    url = "https://github.com/gildor478/ocaml-xdg-basedir/archive/0.0.2.tar.gz";
    sha256 = "0xm3ybz4i84lcwhpamjx4g2yj6lf9avlj1fbrl8idam80wmqfqxj";
  };

  buildInputs = [ ocaml findlib ounit ];
  propagatedBuildInputs = [ fileutils ];
  createFindlibDestdir = true;
}
