{
	ocaml-ng, ocamlPackagesOverride ? ocaml-ng.ocamlPackages_4_10,
	# sources from nix-wrangle:
	self ? ../.,
	opam, cudf, dose, mccs, opam-file-format
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
		opam-core = callOcamlPackage opam.core {};
		opam-format = callOcamlPackage opam.format {};
		opam-installer = callOcamlPackage opam.installer {};
		opam-repository = callOcamlPackage opam.repository {};
		opam-solver = callOcamlPackage opam.solver {};
		opam-state = callOcamlPackage opam.state {};
		opam-client = callOcamlPackage opam.client {};

		opam-file-format = callOcamlPackage ({stdenv, ocaml, findlib }:
			stdenv.mkDerivation {
				name = "opam-file-format";
				src = opam-file-format;
				buildInputs = [ocaml findlib];
				createFindlibDestdir = true;
				installPhase = "make install PREFIX=$out LIBDIR=$out/lib/ocaml/${ocaml.version}/site-lib/";
			}
		) {};

		cudf = callOcamlPackage ({stdenv, ocaml, ocamlbuild, camlp4, findlib, ocaml_extlib, perl}:
			stdenv.mkDerivation {
				name = "cudf";
				src = cudf;
				buildInputs = [ocaml findlib ocamlbuild perl];
				propagatedBuildInputs = [ocaml_extlib];
				patchPhase = "sed -i s@/usr/@$out/@ Makefile.config";
				buildPhase = "make all opt";
				createFindlibDestdir = true;
			}) {};

		dose3 = callOcamlPackage (
			{stdenv, ocaml, findlib, ocamlbuild, camlp4, ocaml_extlib, cudf, ocamlgraph, cppo, re, perl}:
			stdenv.mkDerivation {
				name = "dose";
				src = dose;
				buildInputs = [
					ocaml findlib ocamlbuild
					ocaml_extlib cudf ocamlgraph cppo re perl
				];
				createFindlibDestdir = true;
				patches = [
					./dose/compilation.diff
					./dose/0001-Install-mli-cmx-etc.patch
					./dose/0002-dont-make-printconf.patch
					./dose/0003-Fix-for-ocaml-4.06.patch
					./dose/0004-Add-unix-as-dependency-to-dose3.common-in-META.in.patch
				];
			}
		) {};

		mccs = callOcamlPackage (
			{buildDunePackage, ocaml, dune, findlib, cudf, ocaml_extlib}:
			buildDunePackage rec {
				version = "dev";
				pname = "mccs";
				src = mccs;
				buildInputs = [ findlib ocaml dune cudf ocaml_extlib ];
				configurePhase = "true";
			}
		) {};

		ocamlgraph = callOcamlPackage <nixpkgs/pkgs/development/ocaml-modules/ocamlgraph/default.nix> { lablgtk = null; };
	};

in callOcamlPackage ./opam2nix.nix { inherit self; }
