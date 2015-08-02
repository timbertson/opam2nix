defs: { pkgs }:
with pkgs; with lib;
defs // {
	ocamlfind = mapAttrs (version: def: def.withOverride (import ./ocamlfind {inherit pkgs;})) defs.ocamlfind;
}
