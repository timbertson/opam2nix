{ stdenv, lib, nix, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state, ocaml_lwt, lwt_ppx, ocurl, yojson, fileutils, basedir, gup, ounit, makeWrapper, dune, ocaml-migrate-parsetree,
coreutils, nix-update-source }:
let
version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
self = stdenv.mkDerivation {
	name = "opam2nix-${version}";
	src = ../.;
	buildPhase = "gup release";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
		wrapProgram $out/bin/opam2nix \
			--prefix PATH : "${opam-installer}/bin" \
			--prefix PATH : "${nix.out}/bin" \
		;
	'';
	passthru = let
		makeApi = args: callPackage ./api.nix ({ opam2nix = self; } // args);
		defaultApi = makeApi {};
	in ({
		format_version = import ./format_version.nix;
		devInputs = [ utop ];
		# expose ability to re-make API with nondefault params
		api = makeApi;
	}) // defaultApi;
	buildInputs = [
		ocaml
		findlib
		opam-solver
		opam-state
		opam-installer
		nix
		ocaml_lwt
		(ocurl.overrideAttrs (o: {
			propagatedBuildInputs = (o.propagatedBuildInputs or []) ++ [ocaml_lwt lwt_ppx];
		}))
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
