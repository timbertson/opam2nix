{pkgs ? import <nixpkgs> {}}:
with pkgs;
let version = "0.5.0"; in
stdenv.mkDerivation {
	name = "gup-${version}";
	src = fetchurl {
		url = "https://github.com/gfxmonk/gup/archive/version-${version}.tar.gz";
		sha256 = "0jd3k5lcm7xwm3pmgxq6kp0rchlri8ivc8y445qdz1g2cw7lqw85";
	};
	SKIP_PYCHECKER = "1";
	buildPhase = "make -C python bin/gup";
	buildInputs = [python];
	installPhase = ''
		mkdir "$out"
		cp -a python/bin "$out/bin"
	'';
}

