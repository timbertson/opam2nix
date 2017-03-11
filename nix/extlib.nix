{stdenv, fetchurl, ocaml, findlib, camlp4, cppo, minimal ? true}:

assert stdenv.lib.versionAtLeast (stdenv.lib.getVersion ocaml) "3.11";

stdenv.mkDerivation {
  name = "ocaml-extlib-1.7.1";

  src = fetchurl {
    url = http://ygrek.org.ua/p/release/ocaml-extlib/extlib-1.7.1.tar.gz;
    sha256 = "1lqk4grj0xnb9vahj6dy06c7x19p6djkp02kin192wgrcdswjqgh";
  };

  buildInputs = [ocaml findlib camlp4 cppo];

  createFindlibDestdir = true;

  # configurePhase = "true";      # Skip configure
  # # De facto, option minimal=1 seems to be the default.  See the README.
  # buildPhase     = "make ${if minimal then "minimal=1" else ""} build";
  # installPhase   = "make ${if minimal then "minimal=1" else ""} install";

  meta = {
    homepage = http://code.google.com/p/ocaml-extlib/;
    description = "Enhancements to the OCaml Standard Library modules";
    license = stdenv.lib.licenses.lgpl21;
    platforms = ocaml.meta.platforms or [];
  };
}
