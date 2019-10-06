{ pkgs, selection, super }:
let
	ocaml_version = selection.ocaml.version;
in super.overrideAttrs (impl: {
	# prePatch = (impl.prePatch or "") + "; rm -rf /tmp/findlibby; cp -r src/findlib /tmp/findlibby";
	# patches = (impl.patches or []) ++ [ ./install_topfind.patch ];
	postPatch = ''
		sed -i -e '/INSTALL_TOPFIND/ s/OCAML_CORE_STDLIB/OCAML_SITELIB/' src/findlib/Makefile
	'';
	# buildPhase = ''
	# 	echo 'ldconf="ignore"' >> findlib.conf.in
	# 	./configure \
	# 		-bindir $out/bin \
	# 		-mandir $out/share/man \
	# 		-sitelib $out/lib \
	# 		-config $out/etc/findlib.conf \
	# 		-no-custom \
	# 	;
	# 	make all
	# 	make opt
	# 	make install
	# '';

	setupHook = pkgs.writeText "setupHook.sh" ''
		findlibPreBuildAction () {
			mkdir -p "''$out/lib"
		}

		findlibSetup () {
			base="$(dirname "$(dirname ''${BASH_SOURCE[0]})")"
			export OCAMLTOP_INCLUDE_PATH="''${OCAMLTOP_INCLUDE_PATH:+$OCAMLTOP_INCLUDE_PATH:}$base/lib/toplevel"
			export OCAMLFIND_DESTDIR="''$out/lib/"
			if [[ $preBuildPhases != *findlibPreBuildAction* ]]; then
				export preBuildPhases="''${preBuildPhases:+$preBuildPhases }findlibPreBuildAction"
			fi
		}

		addEnvHooks "$targetOffset" findlibSetup
	'';
})

