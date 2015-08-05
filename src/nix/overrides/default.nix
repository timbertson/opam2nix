defs: { pkgs }:
with pkgs; with lib;
let
	overrideAll = fn: versions: mapAttrs (version: def: def.withOverride fn) versions;
in
defs // {
	ocamlfind = overrideAll (import ./ocamlfind {inherit pkgs;}) defs.ocamlfind;
}
