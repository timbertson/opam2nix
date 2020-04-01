{ stdenv, lib, nix, targetPackages, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state,
ocaml_lwt, lwt_ppx, ocurl, yojson, fileutils,
gup, ounit, makeWrapper, dune, ocaml-migrate-parsetree,
coreutils, nix-update-source, self }:
let
version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
opam2nix = stdenv.mkDerivation {
	name = "opam2nix-${version}";
	src = self;
	buildPhase = "gup release";
	installPhase = ''
		mkdir $out
		cp -r --dereference bin $out/bin
		wrapProgram $out/bin/opam2nix \
			--prefix PATH : "${lib.makeBinPath [ opam-installer nix targetPackages.git ]}" \
		;
	'';
	passthru = let
		makeApi = args: callPackage ./api.nix ({ inherit opam2nix; } // args);
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
		gup
		ounit
		makeWrapper
		dune
		ocaml-migrate-parsetree
		coreutils
	];
};
in
opam2nix
