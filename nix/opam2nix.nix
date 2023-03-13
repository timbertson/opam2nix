{ lib, nix, targetPackages, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state,
opam-0install, buildDunePackage,
ppx_deriving, lwt, lwt_ppx, ocurl, yojson, ppx_deriving_yojson, fileutils,
gup, ounit, makeWrapper, ocaml-migrate-parsetree-2,
coreutils, nix-update-source, opam2nixSrc }:
let
version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
opam2nix = buildDunePackage {
	pname = "opam2nix";
	inherit version;
	src = opam2nixSrc;
	duneVersion = "3";
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
		lwt
		lwt_ppx
		ocurl
		yojson
		ppx_deriving
		ppx_deriving_yojson
		fileutils
		gup
		ounit
		makeWrapper
		ocaml-migrate-parsetree-2
		coreutils
	];
};
in
opam2nix
