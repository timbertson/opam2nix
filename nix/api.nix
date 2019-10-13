{ lib, stdenv, pkgs, makeSetupHook, opam2nix }:

with builtins;
with lib;
let
	isPseudo = impl: elem (typeOf impl) ["null" "bool"];
	nonPseudoList = filter (impl: !isPseudo impl);
	nonPseudoAttrs = filterAttrs (name: impl: !isPseudo impl);
	noopOverride = {}: {};
in
rec {
	build = { deps, ocaml,
		override ? noopOverride,
		builtinOverride ? ./overrides,
		src ? false
	}: let
		# src can either be a plain attribute set, in which case we lookup each
		# direct source by name. If it's a plain object (path or derivation),
		# we use it for all (presumably just one) direct sources.
		directSrc = name:
			let
				validate = src: if src == false
					then abort "Source for direct package `${name}` not provided in `src` argument"
					else src;
				getAttrOrNull = name: attrs: if hasAttr name attrs then getAttr name attrs else null;
				isDrv = hasAttr "outPath"; # builtins.fetchgit isn't a true derivation but has outPath
			in
			validate (if isAttrs src && !isDrv src
				then getAttrOrNull name src
				else src);

		self = {
			inherit lib pkgs repoPath selection directSrc;
		};

		depFn = if isFunction deps then deps else import deps;
		imported = let raw = depFn self; in
			if raw.ocaml-version != ocaml.version then
				abort ("Dependencies were selected for ocaml version ${raw.ocaml-version}" +
					" but we are building with ${ocaml.version}")
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

		opam2nixHooks = makeSetupHook { name = "ocaml-path-hooks"; } ./overrides/path-hooks.sh;
		invoke = "${opam2nix}/bin/opam2nix invoke";
		builtSelection = ({ inherit ocaml; }) // (mapAttrs (name: args:
		if isPseudo args then args else stdenv.mkDerivation (
			{
				inherit (args) pname version src;
				propagatedBuildInputs = [ocaml opam2nix opam2nixHooks] ++ (
					nonPseudoList ((args.buildInputs or []) ++ (attrValues args.opamInputs))
				);
				prePatch = "${invoke} patch";
				buildPhase = "${invoke} build";
				configurePhase = "true";
				installPhase = "${invoke} install";
				opamEnv = builtins.toJSON {
					inherit (args) version opamSrc;
					name = args.pname;
					deps = mapAttrs (name: impl:
						if isPseudo impl then impl else {
							path = impl;
							inherit (impl) version;
						}
					) args.opamInputs;
				};
			}
			// (if args.src == null then { unpackPhase = "true"; } else {})
			// (args.drvAttrs or {})
		)) imported.selection);

		initOverride = pkgs.newScope { inherit opam2nix selection; };

		applyOverride = override: selection:
			let overrideAttrs = initOverride override {}; in
			mapAttrs (name: base:
			if hasAttr name overrideAttrs
				then (getAttr name overrideAttrs) base
				else base
			) selection;

		selection = applyOverride override (
			applyOverride ./overrides builtSelection
		);
		in
		nonPseudoAttrs selection;

	buildInputs = args: attrValues (build args);
}
