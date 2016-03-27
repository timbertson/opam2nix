{stdenv, fetchurl, boost, cmake, re2c, gringo, clasp}:

stdenv.mkDerivation rec {
  version="1.9.1";
  name = "aspcud-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/potassco/files/aspcud/${version}/aspcud-${version}-source.tar.gz";
    sha256 = "09sqbshwrqz2fvlkz73mns5i3m70fh8mvwhz8450izy5lsligsg0";
  };

  buildInputs = [ cmake boost re2c ];
  cmakeFlags = [
    "-DGRINGO_LOC=${gringo}/bin/gringo"
    "-DCLASP_LOC=${clasp}/bin/clasp"
  ];
}
