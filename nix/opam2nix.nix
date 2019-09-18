{ stdenv, lib, nix, callPackage,
ocaml, findlib, utop, opam-installer, opam-solver, opam-state, ocaml_lwt, ocurl, yojson, fileutils, basedir, gup, ounit, makeWrapper, dune, ocaml-migrate-parsetree,
coreutils, nix-update-source }:
let
origin =
	let isStorePath = x: lib.isStorePath (builtins.toString x); in # workaround https://github.com/NixOS/nixpkgs/issues/48743
	if isStorePath ../. then {
		src = ../.;
		version = lib.removeSuffix "\n" (builtins.readFile ../VERSION);
	} else lib.warn "Importing opam2nix src from ${./src.json} since ${builtins.toString ../.} is not a store path" {
		inherit (nix-update-source.fetch ./src.json) src version;
	};
self = stdenv.mkDerivation {
	name = "opam2nix-${origin.version}";
	src = origin.src;
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
