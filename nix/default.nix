{
	ocaml-ng, ocamlPackagesOverride ? ocaml-ng.ocamlPackages_4_10,
	# sources from nix-wrangle:
	self ? ../.,
	opam, opam-file-format, opam-0install-solver, zeroinstall,
}:
let
	opamSrc = opam;
	ocamlPackages = ocamlPackagesOverride;
in
let
	ocaml = ocamlPackages.ocaml;
	opam = callOcamlPackage ./opam.nix { inherit ocamlPackages; src = opamSrc; };
	callOcamlPackage = ocamlPackages.newScope {
		inherit ocaml ocamlPackages;
		dune = ocamlPackages.dune_2;
		opam-core = callOcamlPackage opam.core {};
		opam-format = callOcamlPackage opam.format {};
		opam-installer = callOcamlPackage opam.installer {};
		opam-repository = callOcamlPackage opam.repository {};
		opam-solver = callOcamlPackage opam.solver {};
		opam-state = callOcamlPackage opam.state {};
		opam-client = callOcamlPackage opam.client {};

		zeroinstall-solver = callOcamlPackage ({ buildDunePackage }:
			buildDunePackage {
				useDune2 = true;
				pname = "0install-solver";
				version = "master";
				src = zeroinstall;
			}
		) {};
		
		opam-0install = callOcamlPackage ({ buildDunePackage, fmt, cmdliner, opam-state, zeroinstall-solver }:
			buildDunePackage {
				pname = "opam-0install";
				src = opam-0install-solver;
				version = "master";
				useDune2 = true;
				propagatedBuildInputs = [fmt cmdliner opam-state zeroinstall-solver];
			}
		) {};

		opam-file-format = callOcamlPackage ({stdenv, ocaml, findlib }:
			stdenv.mkDerivation {
				name = "opam-file-format";
				src = opam-file-format;
				buildInputs = [ocaml findlib];
				createFindlibDestdir = true;
				installPhase = "make install PREFIX=$out LIBDIR=$out/lib/ocaml/${ocaml.version}/site-lib/";
			}
		) {};

		ocamlgraph = callOcamlPackage <nixpkgs/pkgs/development/ocaml-modules/ocamlgraph/default.nix> { lablgtk = null; };
	};

in callOcamlPackage ./opam2nix.nix { inherit self; }
