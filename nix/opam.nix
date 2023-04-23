{ pkgs, fetchFromGitHub, ocamlPackages, src }:
let
	base = name: {
		propagatedBuildInputs ? [],
		buildInputs ? [],
		duneVersion ? "3",
		... } @ attrs: attrs // {
		pname = "opam-${name}";
		version = "dev";
		inherit duneVersion buildInputs propagatedBuildInputs src;
		configureFlags = ["--disable-checks"];
	};
in
{
	core = { cppo, ocamlgraph, re, cmdliner, uutf, swhid_core, jsonm, sha }: ocamlPackages.buildDunePackage (base "core" {
		propagatedBuildInputs = [ ocamlgraph re uutf swhid_core jsonm sha ];
		buildInputs = [cppo cmdliner ];
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

	state = { opam-repository, spdx_licenses }: ocamlPackages.buildDunePackage (base "state" {
		propagatedBuildInputs = [ opam-repository spdx_licenses ];
	});
}
