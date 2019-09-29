function ocamlPathSetup {
	if test -d "$1/lib"; then
		export OCAMLPATH="${OCAMLPATH}${OCAMLPATH:+:}$1/lib"

		if test -d "$1/lib/stublibs"; then
			export CAML_LD_LIBRARY_PATH="${CAML_LD_LIBRARY_PATH}${CAML_LD_LIBRARY_PATH:+:}$1/lib/stublibs"
		else
			export CAML_LD_LIBRARY_PATH="${CAML_LD_LIBRARY_PATH}${CAML_LD_LIBRARY_PATH:+:}$1/lib"
		fi
	fi
}
addEnvHooks "$targetOffset" ocamlPathSetup
