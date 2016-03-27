{stdenv, fetchurl, cmake }:

stdenv.mkDerivation rec {
  version="3.1.4";
  name = "clasp-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/potassco/files/clasp/${version}/clasp-${version}-source.tar.gz";
    sha256 = "1zkjqc4gp4n9p2kf3k3z8x82g42any4p3shhhivny89z1jlxi9zn";
  };

  buildInputs = [ cmake ];
  dontUseCmakeConfigure = true;
  configureScript = "./configure.sh";
  preBuild = ''
    mkdir -p $out/bin
    cd build/release
  '';
}
