{stdenv, fetchurl, scons, python, bison, re2c, pkgconfig, libcxx }:

stdenv.mkDerivation rec {
  version="4.5.4";
  name = "gringo-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/potassco/files/gringo/${version}/gringo-${version}-source.tar.gz";
    sha256 = "16k4pkwyr2mh5w8j91vhxh9aff7f4y31npwf09w6f8q63fxvpy41";
  };
  patches = [ ./gringo.patch ];

  postPatch = stdenv.lib.optionalString stdenv.isDarwin ''
    substituteInPlace ./SConstruct \
      --replace \
        "env['CXX']            = 'g++'" \
        "env['CXX']            = '$CXX'"
    substituteInPlace ./SConstruct \
      --replace \
        "env['CPPPATH']        = []" \
        "env['CPPPATH']        = ['${libcxx}/include/c++/v1']"
    substituteInPlace ./SConstruct \
      --replace \
        "env['LIBPATH']        = []" \
        "env['LIBPATH']        = ['${libcxx}/lib']"
  '';

  buildInputs = [ scons bison re2c pkgconfig ];

  buildPhase = ''
    scons --build-dir=release LIBPATH="${python}/lib"
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp -a build/release/{gringo,clingo,reify} $out/bin/
  '';
}
