{ pkgs, selection, super }:
let
	ocaml_version = selection.ocaml.version;
in super.overrideAttrs (impl: {
	postPatch = ''
		sed -i -e '/INSTALL_TOPFIND/ s/OCAML_CORE_STDLIB/OCAML_SITELIB/' src/findlib/Makefile
	'';

	setupHook = pkgs.writeText "setupHook.sh" ''
		findlibPreBuildAction () {
			mkdir -p "''$out/lib/ocaml/${ocaml_version}/site-lib"
		}

		findlibSetup () {
			local base="$(dirname "$(dirname ''${BASH_SOURCE[0]})")"
			if [[ $OCAMLTOP_INCLUDE_PATH != *$base* ]]; then
				export OCAMLTOP_INCLUDE_PATH="''${OCAMLTOP_INCLUDE_PATH:+$OCAMLTOP_INCLUDE_PATH:}$base/lib/ocaml/${ocaml_version}/site-lib/toplevel"
			fi
			if [[ $preBuildPhases != *findlibPreBuildAction* ]]; then
				export preBuildPhases="''${preBuildPhases:+$preBuildPhases }findlibPreBuildAction"
			fi
		}

		addEnvHooks "$targetOffset" findlibSetup
	'';
})

