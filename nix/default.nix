{ ocaml-ng }:
let
	ocamlPackages = ocaml-ng.ocamlPackages_4_06;
	ocaml = ocamlPackages.ocaml;
	opam = callOcamlPackage ./opam.nix { inherit ocamlPackages; };
	callOcamlPackage = ocamlPackages.newScope {
		inherit ocaml;
		ocamlgraph = callOcamlPackage ./ocamlgraph.nix {}; # override builtin drv without lablgtk support
		opam-core = callOcamlPackage opam.core {};
		opam-format = callOcamlPackage opam.format {};
		opam-file-format = callOcamlPackage ./opam-file-format.nix {};
		opam-installer = callOcamlPackage opam.installer {};
		opam-repository = callOcamlPackage opam.repository {};
		opam-solver = callOcamlPackage opam.solver {};
		opam-state = callOcamlPackage opam.state {};
		cudf = callOcamlPackage ./cudf.nix {};
		dose3 = callOcamlPackage ./dose3.nix {};
		mccs = callOcamlPackage ./mccs.nix {};
		basedir = callOcamlPackage ./basedir.nix {};
	};
in callOcamlPackage ./opam2nix.nix {}
