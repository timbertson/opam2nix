{stdenv, fetchurl, ocaml, findlib, ocaml_lwt,
js_of_ocaml, base64, cmdliner, fieldslib, ocaml_sexplib, uri,
stringext, magic-mime, conduit, openssl, libev, ocaml_ipaddr}:

stdenv.mkDerivation {
  name = "ocaml-cohttp";

  src = fetchurl {
    url = "https://github.com/mirage/ocaml-cohttp/archive/v0.18.1.tar.gz";
    sha256 = "1r1x99d39np172l4f75hkrajzn68g9jis14prk1l30x72a7w432b";
  };

  buildInputs = [
    ocaml findlib
  ];

  propagatedBuildInputs = [
    openssl libev
    ocaml_lwt js_of_ocaml base64 cmdliner fieldslib
    ocaml_sexplib uri stringext magic-mime conduit
    ocaml_ipaddr #XXX
  ];
  # doCheck = false;

  # configureScript= "ocaml ./setup.ml -configure";
  preBuild="export PREFIX=\"$prefix\"";
  # buildCommand = "set -x; ls -l; echo $PREFIX; make PREFIX=$prefix; exit 1";
  # PREFIX = "$prefix";

  createFindlibDestdir = true;
}
