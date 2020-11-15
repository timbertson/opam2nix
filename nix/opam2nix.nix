{ lib, nix, targetPackages, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state,
opam-0install, buildDunePackage,
ocaml_lwt, lwt_ppx, ocurl, yojson, fileutils,
gup, ounit, makeWrapper, dune, ocaml-migrate-parsetree,
coreutils, nix-update-source, self }:
let
version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
opam2nix = buildDunePackage {
	pname = "opam2nix";
	inherit version;
	src = self;
	useDune2 = true;
	postInstall = ''
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
		opam-state
		opam-installer
		opam-0install
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
		ocaml-migrate-parsetree
		coreutils
	];
};
in
opam2nix
