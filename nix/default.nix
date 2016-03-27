{ pkgs, stdenv, lib, ocamlPackages, newScope, libev, fetchurl }:
{ src ? null }:
let
	localPackages = lib.makeScope pkgs.newScope (self: with self; pkgs // {
		aspcud = callPackage ./aspcud.nix {};
		clasp = callPackage ./clasp.nix {};
		gringo = callPackage ./gringo.nix {};
		gup = callPackage ./gup.nix {};

		ocamlPackages = lib.makeScope pkgs.newScope (self: with self; pkgs.ocamlPackages // {
			opam-lib = callPackage ./opam-lib.nix {};
			cudf = callPackage ./cudf.nix {};
			dose = callPackage ./dose.nix {};
			basedir = callPackage ./basedir.nix {};
		});
	});

	ocVersion = (builtins.parseDrvName (localPackages.ocamlPackages.ocaml.name)).version;
in
with localPackages; with localPackages.ocamlPackages;
stdenv.mkDerivation {
	name = "opam2nix-${lib.removeSuffix "\n" (builtins.readFile ../VERSION)}";
	inherit src;
	# unpackCmd = "tar xzf $src";
	buildPhase = "gup all";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
		wrapProgram $out/bin/opam2nix \
			--prefix PATH : "${localPackages.aspcud}/bin" \
		;
	'';
	passthru = {
		format_version = import ./format_version.nix;
		pkgs = localPackages;
	};
	buildInputs = [
		ocaml
		findlib
		opam-lib
		ocaml_lwt
		ocurl
		yojson
		fileutils
		basedir
		gup
		ounit
		makeWrapper

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
	CAML_LD_LIBRARY_PATH = lib.concatStringsSep ":" [
		"${ocaml_lwt}/lib/ocaml/${ocVersion}/site-lib/lwt"
		"${ocurl}/lib/ocaml/${ocVersion}/site-lib/curl"
	];
}

