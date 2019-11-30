{ pkgs, fetchFromGitHub, ocamlPackages, src }:
let
	version = "2.0.4";
	base = name: {
		propagatedBuildInputs ? [],
		buildInputs ? [],
		... } @ attrs: attrs // {
		pname = "opam-${name}";
		inherit version buildInputs propagatedBuildInputs src;
		configureFlags = "--disable-checks";
	};
in
{
	core = { cppo, dune, ocamlgraph, re, cmdliner }: ocamlPackages.buildDunePackage (base "core" {
		propagatedBuildInputs = [ ocamlgraph re ];
		buildInputs = [cppo cmdliner];
	});
	client = { opam-state, opam-solver, re, cmdliner}: ocamlPackages.buildDunePackage (base "client" {
		propagatedBuildInputs = [ opam-state opam-solver re cmdliner];
	});
	format = { opam-core, opam-file-format, re}: ocamlPackages.buildDunePackage (base "format" {
		propagatedBuildInputs = [ opam-core opam-file-format re];
	});
	installer = { cmdliner, opam-format }: ocamlPackages.buildDunePackage (base "installer" {
		propagatedBuildInputs = [ cmdliner opam-format ];
	});
	repository = { opam-format }: ocamlPackages.buildDunePackage (base "repository" {
		propagatedBuildInputs = [ opam-format ];
	});
	solver = { cudf, dose3, mccs, opam-format }: ocamlPackages.buildDunePackage (base "solver" {
		propagatedBuildInputs = [ cudf dose3 mccs opam-format ];
	});
	state = { opam-repository }: ocamlPackages.buildDunePackage (base "state" {
		propagatedBuildInputs = [ opam-repository ];
	});
}
