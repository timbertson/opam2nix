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
	resolve = {
		ocaml,
		selection,
		# NOTE: these aren't used, but are explicitly allowed for compatibility with `build` / `buildInputs`
		override ? null,
		builtinOverride ? null,
		src ? null,
	}: args: let
	in pkgs.mkShell {
		buildInputs = [ opam2nix ];
		shellHook = ''
			opam2nix resolve \
				--dest ${builtins.toString selection} \
				--ocaml-version ${ocaml.version} \
				${lib.concatStringsSep " " (map (arg: "'${builtins.toString arg}'") args)}
			{ exit $?; } 2>/dev/null
		'';
	};

	build = { selection, ocaml,
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
			inherit lib pkgs repoPath directSrc;
			selection = finalSelection;
		};

		depFn = if isFunction selection then selection else import selection;
		imported = let raw = depFn self; in
			if raw.ocaml-version != ocaml.version then
				warn ("Dependencies were selected for ocaml version ${raw.ocaml-version}" +
					" but you are building with ${ocaml.version}") raw
			else raw;

		repoPath = repo: { package, hash }:
			stdenv.mkDerivation {
				outputHash = hash;
				outputHashMode = "recursive";
				name = "opam";
				buildCommand = ''
					cp -r "${repo}/${package}" "$out";
					chmod u+w "$out"
				'';
			};

		initOverride = pkgs.newScope {
			inherit opam2nix ocaml;
			selection = finalSelection;
		};

		applyOverride = override: selection:
			let overrideAttrs = initOverride override {}; in
			mapAttrs (name: base:
			if hasAttr name overrideAttrs
				then (getAttr name overrideAttrs) base
				else base
			) selection;

		opam2nixHooks = makeSetupHook { name = "ocaml-path-hooks"; } (pkgs.writeText "setupHook.sh" ''
			function ocamlPathSetup {
				local libPath="lib/ocaml/${ocaml.version}/site-lib"
				local libdir="$1/$libPath"
				if test -d "$libdir"; then
					export OCAMLPATH="''${OCAMLPATH}''${OCAMLPATH:+:}$libdir"

					if test -d "$1/lib/stublibs"; then
						export CAML_LD_LIBRARY_PATH="''${CAML_LD_LIBRARY_PATH}''${CAML_LD_LIBRARY_PATH:+:}$libdir/stublibs"
					else
						export CAML_LD_LIBRARY_PATH="''${CAML_LD_LIBRARY_PATH}''${CAML_LD_LIBRARY_PATH:+:}$libdir"
					fi
				fi
				export OCAMLFIND_DESTDIR="$out/$libPath"
			}
			addEnvHooks "$targetOffset" ocamlPathSetup
		'');
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
					) ({ inherit ocaml; } // args.opamInputs);
				};
			}
			// (if args.src == null then { unpackPhase = "true"; } else {})
			// (args.drvAttrs or {})
		)) imported.selection);

		finalSelection = applyOverride override (
			applyOverride ./overrides builtSelection
		);
		in
		nonPseudoAttrs finalSelection;

	buildInputs = args: attrValues (build args);
}
