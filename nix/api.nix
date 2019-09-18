{ lib, stdenv, pkgs, opam2nix }:

with builtins;
with lib;
let
	isPseudo = impl: elem (typeOf impl) ["null" "bool"];
	nonPseudoList = filter (impl: !isPseudo impl);
	nonPseudoAttrs = filterAttrs (name: impl: !isPseudo impl);
	# TODO this is a weird API
	overrides = {
		"0install" = super:
			if super.version == "123" then super.overrideAttrs (o: { }) else super;
	};
	# TODO: add a pseudo package that adds deps to ocamlpath etc
in
rec {
	build = { deps, ocaml }: let
		depFn = if isFunction deps then deps else import deps;
		imported = let raw = depFn {
				inherit lib pkgs repoPath;
				selection = builtSelection;
			}; in
			if raw.ocaml-version != ocaml.version then
				abort ("Dependencies were selected for ocaml version ${raw.ocaml-version}" +
					"but we are building with ${ocaml.version}")
			else raw;

		repoPath = repo: { package, hash }:
			stdenv.mkDerivation {
				outputHash = hash;
				outputHashMode = "recursive";
				name = "opam";
				buildCommand = ''
					cp -r "${repo}/${package}" "$out";
				'';
			};

		invoke = "${opam2nix}/bin/opam2nix invoke";
		builtSelection = mapAttrs (name: args:
		if isPseudo args then args else stdenv.mkDerivation (
			{
				inherit (args) pname version src;
				buildInputs = [ocaml opam2nix] ++ (
					nonPseudoList (args.buildInputs ++ (attrValues args.opamInputs))
				);
				patchPhase = "${invoke} patch";
				# TODO zip / tgz unpack?
				buildPhase = "${invoke} build";
				installPhase = "${invoke} install";
				opamEnv = builtins.toJSON {
					inherit (args) opamInputs pname version opamSrc;
				};
			}
			// (if args.src == null then { unpackPhase = "true"; } else {})
			// (args.drvAttrs or {})
		)) imported.selection;
		in
		nonPseudoAttrs builtSelection;

	buildInputs = args: attrValues (build args);
}
