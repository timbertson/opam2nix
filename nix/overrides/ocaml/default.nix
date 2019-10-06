# A special snowflake override for ocaml, to interop with opam+findlib.
# Findlib's opam package gets away with it by shadowing the `ocaml` binary on $PATH,
# but that won't work in a nix build environment
{ makeSetupHook, lib, stdenv, bash }: impl:
let
setupHook = makeSetupHook { name = "ocaml-path-hooks"; } ./path-hooks.sh;
propagatedBuildInputs = [ setupHook ];
in
impl.overrideAttrs (orig: {
	propagatedBuildInputs = (orig.propagatedBuildInputs or []) ++ propagatedBuildInputs;
})
