{pkgs}:
impl:
let
	ocaml_version = (builtins.parseDrvName impl.passthru.ocaml.name).version;
in impl // {
	patches = [ ./ldconf.patch ./install_topfind.patch ];
	buildPhase = ''
		./configure \
			-bindir $out/bin \
			-mandir $out/share/man \
			-sitelib $out/lib/ocaml/${ocaml_version}/site-lib \
			-no-topfind \
			-config $out/etc/findlib.conf
		make all
		make opt
		make install
	'';

	setupHook = pkgs.writeText "setupHook.sh" ''
		addOCamlPath () {
				if test -d "''$1/lib/ocaml/${ocaml_version}/site-lib"; then
						export OCAMLPATH="''${OCAMLPATH}''${OCAMLPATH:+:}''$1/lib/ocaml/${ocaml_version}/site-lib/"
				fi
				export OCAMLFIND_DESTDIR="''$out/lib/ocaml/${ocaml_version}/site-lib/"
				if test -n "$createFindlibDestdir"; then
					mkdir -p $OCAMLFIND_DESTDIR
				fi
		}
		envHooks+=(addOCamlPath)
	'';
}

