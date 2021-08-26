{ pkgs, fetchFromGitHub, ocamlPackages, src }:
let
	base = name: {
		propagatedBuildInputs ? [],
		buildInputs ? [],
		useDune2 ? true,
		... } @ attrs: attrs // {
		pname = "opam-${name}";
		version = "dev";
		inherit useDune2 buildInputs propagatedBuildInputs src;
		configureFlags = "--disable-checks";
	};
in
{
	core = { cppo, dune, ocamlgraph, re, cmdliner }: ocamlPackages.buildDunePackage (base "core" {
		propagatedBuildInputs = [ ocamlgraph re ];
		buildInputs = [cppo cmdliner];
	});

	format = { opam-core, opam-file-format, re}: (ocamlPackages.buildDunePackage (base "format" {
		propagatedBuildInputs = [ opam-core opam-file-format re];
	}))
	.overrideAttrs (o: {
		patches = [ ../../opam/0001-OpamFilter-handle-converted-variables-correctly-when.patch ];
	})
	;

	installer = { cmdliner, opam-format }: ocamlPackages.buildDunePackage (base "installer" {
		propagatedBuildInputs = [ cmdliner opam-format ];
	});

	repository = { opam-format }: ocamlPackages.buildDunePackage (base "repository" {
		propagatedBuildInputs = [ opam-format ];
	});

	state = { opam-repository }: ocamlPackages.buildDunePackage (base "state" {
		propagatedBuildInputs = [ opam-repository ];
	});
}
