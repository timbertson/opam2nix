{ pkgs, stdenv, lib, ocamlPackages, callPackage, newScope, libev, fetchurl }:
let
	gup = pkgs.callPackage ./gup.nix {};
	ocamlPackages = pkgs.ocamlPackages // rec {
		opam-lib = ocamlScope ./opam-lib.nix {};
		cudf = ocamlScope ./cudf.nix {};
		dose = ocamlScope ./dose.nix {};
	};
	ocamlScope = newScope ocamlPackages;
	ocVersion = (builtins.parseDrvName (ocamlPackages.ocaml.name)).version;
in
stdenv.mkDerivation {
	name = "opam2nix";
	src = fetchurl {
		url = "https://github.com/gfxmonk/opam2nix/tarball/53e22cecc606fcf7ad87e8f997568501ca5871e5";
		sha256 = "1i2n5v7dp71dsfw1jp8wcvnivaj3kzih3h7vs2nggw1x22h4abl7";
	};
	unpackCmd = "tar xzf $src";
	buildPhase = "gup all";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
	'';
	buildInputs = with ocamlPackages; [
		ocaml
		findlib
		opam-lib
		ocaml_lwt
		ocurl
		yojson
		fileutils
		gup

		# XXX these should be picked up by propagatedBuildInputs
		libev
		dose
		cudf
		ocamlgraph
		re
		jsonm
		ocaml_extlib
	];

	# XXX this seems to be necessary for .byte targets only
	# (but we like those during development / testing).
	# Seems fragile though.
	CAML_LD_LIBRARY_PATH = with ocamlPackages; lib.concatStringsSep ":" [
		"${ocaml_lwt}/lib/ocaml/${ocVersion}/site-lib/lwt"
		"${ocurl}/lib/ocaml/${ocVersion}/site-lib/curl"
	];
}

