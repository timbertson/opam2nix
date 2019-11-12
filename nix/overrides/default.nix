{ pkgs, lib, selection, ocaml, opam2nix }:
with pkgs;
with lib;
let
	addPatches = patches: orig: orig.overrideAttrs (orig: { patches = (orig.patches or []) ++ patches; });
	disableStackProtection = super: super.overrideAttrs (impl: { hardeningDisable = [ "stackprotector" ]; });
in
{
	ocamlfind = super: import ./ocamlfind { inherit pkgs super selection; };
	ocb-stubblr = addPatches [./ocb-stubblr/disable-opam.diff];
	nocrypto = super: (disableStackProtection super).overrideAttrs (super: {
		buildPhase = "export OCAMLRUNPARAM=b; " + super.buildPhase;
	});
	ocaml-base-compiler = super: ocaml;
}
