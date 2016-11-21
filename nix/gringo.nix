{stdenv, fetchurl, scons, python, bison, re2c, pkgconfig }:

stdenv.mkDerivation rec {
  version="4.5.4";
  name = "gringo-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/potassco/files/gringo/${version}/gringo-${version}-source.tar.gz";
    sha256 = "16k4pkwyr2mh5w8j91vhxh9aff7f4y31npwf09w6f8q63fxvpy41";
  };
  patches = [ ./gringo.patch ];

  buildInputs = [ scons bison re2c pkgconfig ];

  buildPhase = ''
    scons --build-dir=release LIBPATH="${python}/lib"
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp -a build/release/{gringo,clingo,reify} $out/bin/
  '';
}
