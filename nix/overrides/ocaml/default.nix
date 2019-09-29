# A special snowflake override for ocaml, to interop with opam+findlib.
# Findlib's opam package gets away with it by shadowing the `ocaml` binary on $PATH,
# but that won't work in a nix build environment
{ makeSetupHook, lib, stdenv, bash }: impl:
let
setupHook = makeSetupHook { name = "ocaml-path-hooks"; } ./path-hooks.sh;
propagatedBuildInputs = [ setupHook ];
wrapScript = ''
		mv $out/bin/ocaml $out/bin/.ocaml.wrapped
		cat > $out/bin/ocaml <<EOF
#!${bash}/bin/bash
if test -n "\$OCAML_TOPLEVEL_PATH"; then
	exec "$out/bin/.ocaml.wrapped" -I "\$OCAML_TOPLEVEL_PATH" "\$@"
else
	exec "$out/bin/.ocaml.wrapped" "\$@"
fi
EOF
		chmod a+x $out/bin/ocaml
	'';
override = drv: drv.overrideAttrs (orig: {
	postInstall = (orig.postInstall or "") + wrapScript;
	propagatedBuildInputs = (orig.propagatedBuildInputs or []) ++ propagatedBuildInputs;
});
copy = drv: stdenv.mkDerivation {
	inherit (drv) name;
	inherit propagatedBuildInputs;
	buildCommand = ''
		cp -a ${drv} $out
		chmod +w $out/bin
	'' + wrapScript;
};
in

# If `ocaml` is a nixpkgs derivation, we can override it.
# Otherwise (e.g a plain derivation loaded from a .drv file),
# we can only make a (wrapped) copy of its contents
if (lib.hasAttr "overrideAttrs" impl)
	then override impl
	else copy impl
