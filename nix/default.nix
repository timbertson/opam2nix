{ ocaml-ng, opam }:
let
	ocamlPackages = ocaml-ng.ocamlPackages_4_06;
	ocaml = ocamlPackages.ocaml;
	opamLib = callOcamlPackage ./opam.nix { inherit ocamlPackages; src = opam; };
	callOcamlPackage = ocamlPackages.newScope {
		inherit ocaml ocamlPackages;
		ocamlgraph = callOcamlPackage ./ocamlgraph.nix {}; # override builtin drv without lablgtk support
		opam-core = callOcamlPackage opamLib.core {};
		opam-format = callOcamlPackage opamLib.format {};
		opam-file-format = callOcamlPackage ./opam-file-format.nix {};
		opam-installer = callOcamlPackage opamLib.installer {};
		opam-repository = callOcamlPackage opamLib.repository {};
		opam-solver = callOcamlPackage opamLib.solver {};
		opam-state = callOcamlPackage opamLib.state {};
		opam-client = callOcamlPackage opamLib.client {};
		cudf = callOcamlPackage ./cudf.nix {};
		dose3 = callOcamlPackage ./dose3.nix {};
		mccs = callOcamlPackage ./mccs.nix {};
		basedir = callOcamlPackage ./basedir.nix {};
	};
in callOcamlPackage ./opam2nix.nix {}
