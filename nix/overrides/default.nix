{ pkgs, lib, selection, ocaml, opam2nix }:
with pkgs;
with lib;
let
	addPatches = patches: orig: orig.overrideAttrs (orig: { patches = (orig.patches or []) ++ patches; });
	disableStackProtection = super: super.overrideAttrs (impl: { hardeningDisable = [ "stackprotector" ]; });
in
{
	camlp4 = super: super.overrideAttrs (super: {
		opamSrc = stdenv.mkDerivation {
			name = "camlp4-opam";
			buildCommand = ''
				cp -r ${super.opamSrc} $out
				chmod u+w -R $out
				substituteInPlace $out/opam \
					--replace '--libdir=%{lib}%/ocaml' '--libdir=%{lib}%'
			'';
		};
		configurePhase = (super.configurePhase or "") + '';
			substituteInPlace camlp4/META.in \
			--replace +camlp4 $out/lib/ocaml/${ocaml.version}/site-lib/camlp4
		'';
	});
	ocaml-base-compiler = super: ocaml;
	ocamlfind = super: import ./ocamlfind { inherit pkgs super selection; };
	ocb-stubblr = addPatches [./ocb-stubblr/disable-opam.diff];
	nocrypto = super: (disableStackProtection super).overrideAttrs (super: {
		buildPhase = "export OCAMLRUNPARAM=b; " + super.buildPhase;
	});
}
