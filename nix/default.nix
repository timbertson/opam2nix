{ pkgs, stdenv, lib, ocamlPackages, nix-update-source, newScope, libev, fetchurl }:
let
	localPackages = lib.makeScope pkgs.newScope (self: with self; pkgs // {
		aspcud = callPackage ./aspcud.nix {};
		gringo = callPackage ./gringo.nix {};
		gup = callPackage ./gup.nix {};

		ocamlPackages = lib.makeScope pkgs.newScope (self: with self; ocamlPackages // {
			opam-lib = callPackage ./opam-lib.nix {};
			cudf = callPackage ./cudf.nix {};
			dose3 = callPackage ./dose3.nix {};
			ocamlgraph = callPackage ./ocamlgraph.nix {};
			basedir = callPackage ./basedir.nix {};
			ocaml_extlib = callPackage ./extlib.nix {};
		});
	});

	ocVersion = (builtins.parseDrvName (localPackages.ocamlPackages.ocaml.name)).version;
in
with localPackages; with localPackages.ocamlPackages;
stdenv.mkDerivation {
	name = "opam2nix-${lib.removeSuffix "\n" (builtins.readFile ../VERSION)}";
	inherit (nix-update-source.fetch ./src.json) src;
	# unpackCmd = "tar xzf $src";
	buildPhase = "gup all";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
		wrapProgram $out/bin/opam2nix \
			--prefix PATH : "${localPackages.aspcud}/bin" \
			--prefix PATH : "${pkgs.nix.out}/bin" \
		;
	'';
	passthru = {
		format_version = import ./format_version.nix;
		pkgs = localPackages;
		devInputs = [ utop ];
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
		jbuilder
		ocaml-migrate-parsetree

		# XXX these should be picked up by propagatedBuildInputs
		libev
		camlp4
		cmdliner
		dose3
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

