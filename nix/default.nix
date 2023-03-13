{
	pkgs,
	ocaml-ng, ocamlPackagesOverride ? ocaml-ng.ocamlPackages_4_13,
}:
let
	sources = pkgs.callPackage ./sources.nix { sourcesFile = ./sources.json; };
	ocamlPackages = ocamlPackagesOverride;
in
let
	ocaml = ocamlPackages.ocaml;
	opam = callOcamlPackage ./opam.nix { inherit ocamlPackages; src = sources.opam; };
	callOcamlPackage = ocamlPackages.newScope {
		inherit ocaml ocamlPackages;
		dune_2 = ocamlPackages.dune_2;
		fileutils = ocamlPackages.fileutils.overrideAttrs (o: {
			# disable tests, workaround for https://github.com/timbertson/opam2nix/issues/47
			configureFlags = [];
			doCheck = false;
		});
		opam-core = callOcamlPackage opam.core {};
		opam-format = callOcamlPackage opam.format {};
		opam-installer = callOcamlPackage opam.installer {};
		opam-repository = callOcamlPackage opam.repository {};
		opam-solver = callOcamlPackage opam.solver {};
		opam-state = callOcamlPackage opam.state {};

		zeroinstall-solver = callOcamlPackage ({ buildDunePackage }:
			buildDunePackage {
				useDune2 = true;
				pname = "0install-solver";
				version = "master";
				src = sources.zeroinstall;
			}
		) {};
		
		opam-0install = callOcamlPackage ({ buildDunePackage, fmt, cmdliner, opam-state, zeroinstall-solver }:
			buildDunePackage {
				pname = "opam-0install";
				src = sources.opam-0install-solver;
				version = "master";
				useDune2 = true;
				propagatedBuildInputs = [fmt cmdliner opam-state zeroinstall-solver];
			}
		) {};

		opam-file-format = callOcamlPackage ({buildDunePackage}:
			buildDunePackage {
				pname = "opam-file-format";
				version = "dev";
				src = sources.opam-file-format;
				useDune2 = true;
			}
		) {};

		spdx_licenses = callOcamlPackage ({buildDunePackage}:
			buildDunePackage {
				pname = "spdx_licenses";
				version = "main";
				src = sources.spdx_licenses;
				useDune2 = true;
			}
		) {};
	};

in callOcamlPackage ./opam2nix.nix {
	opam2nixSrc = sources.local { url = ../.; ref = "HEAD"; };
}
