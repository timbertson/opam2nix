{stdenv, fetchurl, ocaml, findlib, stringext, uri, cstruct, ocaml_ipaddr, ocaml_lwt, ocaml_dns, mirage-types, tcpip }:

stdenv.mkDerivation {
  name = "ocaml-conduit";

  src = fetchurl {
    url = "https://github.com/mirage/ocaml-conduit/archive/v0.8.4.tar.gz";
    sha256 = "0fh11qjs6x2y8y6567jgqfafv4nvi16vfz53gx4iimgw2jmawb8f";
  };

  buildInputs = [ ocaml findlib stringext uri cstruct ocaml_ipaddr ocaml_lwt ocaml_dns mirage-types tcpip ];

  propagatedBuildInputs = [ ocaml_ipaddr cstruct ];

  createFindlibDestdir = true;
}
