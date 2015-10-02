{ pkgs, stdenv, lib, ocamlPackages, callPackage, newScope, libev, fetchurl }:
{ src ? null }:
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
	inherit src;
	# unpackCmd = "tar xzf $src";
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
		pkgs.libssh2
	];

	# XXX this seems to be necessary for .byte targets only
	# (but we like those during development / testing).
	# Seems fragile though.
	CAML_LD_LIBRARY_PATH = with ocamlPackages; lib.concatStringsSep ":" [
		"${ocaml_lwt}/lib/ocaml/${ocVersion}/site-lib/lwt"
		"${ocurl}/lib/ocaml/${ocVersion}/site-lib/curl"
	];
}

