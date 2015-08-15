defs: { pkgs }:
with pkgs; with lib;
let
	overrideAll = fn: versions: mapAttrs (version: def: def.withOverride fn) versions;

	# XXX it's not really a `configure` phase, is it?
	addBinDir = def: overrideAll (impl: impl // { configurePhase = ''mkdir -p $out/bin''; }) def;
in
defs // {
	ocamlfind = overrideAll (import ./ocamlfind {inherit pkgs;}) defs.ocamlfind;

	cstruct = overrideAll (impl: impl // {
		installPhase = "make install JS_DEST=$OCAMLFIND_DESTDIR";
	}) defs.cstruct;

	# TODO: should this be automated?
	biniou = addBinDir defs.biniou;
	yojson = addBinDir defs.yojson;
}
