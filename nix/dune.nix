{stdenv, fetchurl, ocaml, }:
stdenv.mkDerivation {
	name = "dune";
	src = fetchurl {
		url = "https://github.com/ocaml/dune/releases/download/1.0.0/dune-1.0.0.tbz";
		sha256 = "18jck4b04p4gyif2nbc091c1c7rdj4gyjj2whgld1kzv9pc56a3k";
	};
	unpackCmd = "tar xjf $src";
	configurePhase = ''
		ocaml configure.ml
		'';
	buildPhase = ''
		ocaml bootstrap.ml
		./boot.exe --release
	'';
	installPhase = ''
		mkdir $out
		_build/install/default/bin/dune install dune --prefix "$out"
	'';
	buildInputs = [ ocaml ];
	createFindlibDestdir = true;
}


