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

	gmp-xen = overrideAll (impl: impl // {
		# this is a plain C lib
		configurePhase = "unset OCAMLFIND_DESTDIR";
	}) defs.gmp-xen;

	zarith-xen = overrideAll (impl: impl // {
		buildPhase = "${pkgs.bash}/bin/bash ${./zarith-xen}/install.sh";
	}) defs.zarith-xen;

	"0install" = overrideAll (impl:
		# disable tests, beause they require additional setup
		let deps = lib.remove impl.passthru.opamSelection.ounit impl.buildInputs; in
		impl // { buildInputs = deps; propagatedBuildInputs = deps; }
	) defs."0install";

	# TODO: should this be automated?
	biniou = addBinDir defs.biniou;
	yojson = addBinDir defs.yojson;
	fat-filesystem = addBinDir defs.fat-filesystem;
}
