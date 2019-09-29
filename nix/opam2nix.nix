{ stdenv, lib, nix, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state, ocaml_lwt, ocurl, yojson, fileutils, basedir, gup, ounit, makeWrapper, dune, ocaml-migrate-parsetree,
coreutils, nix-update-source }:
let
version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
self = stdenv.mkDerivation {
	name = "opam2nix-${version}";
	src = null;
	buildPhase = "gup release";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
		wrapProgram $out/bin/opam2nix \
			--prefix PATH : "${opam-installer}/bin" \
			--prefix PATH : "${nix.out}/bin" \
		;
	'';
	passthru = {
		format_version = import ./format_version.nix;
		devInputs = [ utop ];
		api = callPackage ./api.nix { opam2nix = self; };
	};
	buildInputs = [
		ocaml
		findlib
		opam-solver
		opam-state
		ocaml_lwt
		ocurl
		yojson
		fileutils
		basedir
		gup
		ounit
		makeWrapper
		dune
		ocaml-migrate-parsetree
		coreutils
	];
};
in
self
