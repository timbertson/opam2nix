{ stdenv, pkgs, fetchFromGitHub, ocamlPackages }:
let base = name: {
		propagatedBuildInputs ? [],
		buildInputs ? [],
		... } @ attrs: attrs // {
	name = "opam-${name}"; # TODO: name individually
	src = fetchFromGitHub {
		owner = "ocaml";
		repo = "opam";
		rev = "2.0.0-rc3";
		sha256 = "07zzabv8qrgqglzxm3jkb33byfjvsrimly5m1jgi6m9mdcqzp8wb";
	};
	createFindlibDestdir = true;
	propagatedBuildInputs = [ ocamlPackages.findlib] ++ propagatedBuildInputs;
	buildInputs = [ ocamlPackages.ocaml ocamlPackages.dune ] ++ buildInputs;
	buildFlags = "opam-${name}.install";
	installPhase = "dune install -p opam-${name} --prefix $out opam-${name} || (ls -lr; env; exit 1)";
	configureFlags = "--disable-checks";
};
in
{
	core = { cppo, dune, ocamlgraph, re, cmdliner }: stdenv.mkDerivation (base "core" {
		propagatedBuildInputs = [ ocamlgraph re ];
		buildInputs = [cppo cmdliner];
	});
	format = { opam-core, opam-file-format, re}: stdenv.mkDerivation (base "format" {
		propagatedBuildInputs = [ opam-core opam-file-format re];
	});
	installer = { cmdliner, opam-format }: stdenv.mkDerivation (base "installer" {
		propagatedBuildInputs = [ cmdliner opam-format ];
	});
	repository = { opam-format }: stdenv.mkDerivation (base "repository" {
		propagatedBuildInputs = [ opam-format ];
	});
	solver = { cudf, dose3, mccs, opam-format }: stdenv.mkDerivation (base "solver" {
		propagatedBuildInputs = [ cudf dose3 mccs opam-format ];
	});
	state = { opam-repository }: stdenv.mkDerivation (base "state" {
		propagatedBuildInputs = [ opam-repository ];
	});
}
