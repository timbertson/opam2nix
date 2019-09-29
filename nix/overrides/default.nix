{ pkgs, lib, selection, opam2nix }:
# self has selections & packages
with pkgs;
with lib;
let
	# overrideAll = fn: versions: mapAttrs (version: def: def.overrideAttrs fn) versions;
	# addPatches = patches: orig: { patches = (orig.patches or []) ++ patches; };
	# patchAll = patches: overrideAll (addPatches patches);
	# overrideIf = predicate: fn: versions:
	# 	mapAttrs (version: def:
	# 		if predicate version then
	# 			def.overrideAttrs fn
	# 		else def
	# 	) versions;
  #
	# # XXX it's not really a `configure` phase, is it?
	# addNcurses = def: overrideAll (impl: { buildInputs = impl.buildInputs ++ [ncurses]; }) def;
	# disableStackProtection = def: overrideAll (impl: { hardeningDisable = [ "stackprotector" ]; }) def;
	# opamPackages = super.opamPackages;
in
{
	ocaml = super: pkgs.callPackage ./ocaml {} super;
	ocamlfind = super: import ./ocamlfind { inherit pkgs super selection; };
	# TODO add remaining overrides
# 	camlzip = overrideAll (impl: {
# 		buildInputs = impl.buildInputs ++ [ zlib.dev ];
# 	}) opamPackages.camlzip;
#
# 	ctypes =
# 	let
# 		base =
# 			overrideAll (impl: {
# 				nativeBuildInputs = (impl.nativeBuildInputs or []) ++ [ pkgconfig ];
# 				buildInputs = impl.buildInputs ++ [ libffi ncurses ];
# 			}) opamPackages.ctypes;
# 		withHeaderPatch =
# 		overrideIf (version: lib.elem version [
# 			# hardcode recent versions prior to https://github.com/ocamllabs/ocaml-ctypes/pull/557
# 			"0.11.4"
# 			"0.11.5"
# 			"0.12.0"
# 			"0.12.1"
# 			"0.13.0"
# 			"0.13.1"
# 		]) (addPatches [./ctypes/install-headers-once.patch]) base;
# 	in withHeaderPatch;
#
# 	gmp-xen = overrideAll (impl: {
# 		# this is a plain C lib
# 		configurePhase = "unset OCAMLFIND_DESTDIR";
# 	}) opamPackages.gmp-xen;
#
# 	lablgtk = overrideAll (impl: {
# 		nativeBuildInputs = (impl.nativeBuildInputs or []) ++ [ pkgconfig ];
# 		buildInputs = impl.buildInputs ++ [ gtk2.dev ];
# 	}) opamPackages.lablgtk;
#
# 	llvm = overrideAll (impl: {
# 		nativeBuildInputs = (impl.nativeBuildInputs or []) ++ [ pkgconfig python ];
# 		propagatedBuildInputs = impl.propagatedBuildInputs ++ [ llvm_5 ];
# 		installPhase = ''
# 			bash -ex install.sh ${llvm_5}/bin/llvm-config $out/lib ${cmake}/bin/cmake make
# 			'';
# 	}) opamPackages.llvm;
#
# 	lwt = overrideAll (impl: {
# 		buildInputs = impl.buildInputs ++ [ ncurses ];
# 		setupHook = writeText "setupHook.sh" ''
# 			export LD_LIBRARY_PATH="$(dirname "$(dirname ''${BASH_SOURCE[0]})")/lib/lwt''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
# 		'';
# 	}) opamPackages.lwt;
#
# 	nocrypto = disableStackProtection opamPackages.nocrypto;
# 	ocp-build = addNcurses opamPackages.ocp-build;
# 	ocb-stubblr = patchAll [./ocb-stubblr/disable-opam.diff] opamPackages.ocb-stubblr;
#
# 	# fallout of https://github.com/ocaml/opam-repository/pull/6657
# 	omake = addNcurses opamPackages.omake;
#
# 	piqilib = overrideAll (impl: {
# 		nativeBuildInputs = (impl.nativeBuildInputs or []) ++ [ which makeWrapper ];
# 		# hack -- for some reason the makefile system ignores OCAMLPATH.
# 		configurePhase = ''
# 			mkdir .bin
# 			makeWrapper $(which ocamlfind) .bin/ocamlfind --prefix OCAMLPATH : "$OCAMLPATH"
# 			export PATH=$(readlink -f .bin):$PATH
# 		''+impl.configurePhase;
# 	}) opamPackages.piqilib;
#
# 	solo5-kernel-vertio = disableStackProtection opamPackages.solo5-kernel-vertio;
# 	solo5-kernel-ukvm = disableStackProtection opamPackages.solo5-kernel-ukvm;
#
# 	zarith = overrideAll (impl: {
# 		nativeBuildInputs = (impl.nativeBuildInputs or []) ++ [ perl ];
# 		configurePhase = ''
# 			patchShebangs .
# 		''+impl.configurePhase;
# 	}) opamPackages.zarith;
#
# 	zarith-xen = overrideAll (impl: {
# 		buildPhase = "${pkgs.bash}/bin/bash ${./zarith-xen/install.sh}";
# 		installPhase = "true";
# 	}) opamPackages.zarith-xen;
# };
}
