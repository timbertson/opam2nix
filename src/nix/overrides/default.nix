defs: { pkgs }:
with pkgs; with lib;
let
	overrideAll = fn: versions: mapAttrs (version: def: def.withOverride fn) versions;
in
defs // {
	ocamlfind = overrideAll (import ./ocamlfind {inherit pkgs;}) defs.ocamlfind;

	cstruct = overrideAll (impl: impl // {
		installPhase = "make install JS_DEST=$OCAMLFIND_DESTDIR";
	}) defs.cstruct;
}
