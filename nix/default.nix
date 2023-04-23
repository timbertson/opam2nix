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
				duneVersion = "3";
				pname = "0install-solver";
				version = "master";
				src = sources.zeroinstall;
			}
		) {};

		swhid_core = callOcamlPackage ({ buildDunePackage }:
			buildDunePackage {
				duneVersion = "3";
				pname = "swhid_core";
				version = "master";
				src = sources.swhid_core;
			}
		) {};
		
		opam-0install = callOcamlPackage ({ buildDunePackage, fmt, cmdliner, opam-state, zeroinstall-solver }:
			buildDunePackage {
				pname = "opam-0install";
				src = sources.opam-0install-solver;
				version = "master";
				duneVersion = "3";
				propagatedBuildInputs = [fmt cmdliner opam-state zeroinstall-solver];
			}
		) {};

		opam-file-format = callOcamlPackage ({buildDunePackage}:
			buildDunePackage {
				pname = "opam-file-format";
				version = "dev";
				src = sources.opam-file-format;
				duneVersion = "3";
			}
		) {};

		spdx_licenses = callOcamlPackage ({buildDunePackage}:
			buildDunePackage {
				pname = "spdx_licenses";
				version = "main";
				src = sources.spdx_licenses;
				duneVersion = "3";
			}
		) {};
	};

in callOcamlPackage ./opam2nix.nix {
	opam2nixSrc = sources.local { url = ../.; ref = "HEAD"; };
}
