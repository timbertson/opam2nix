{stdenv, fetchurl, ocaml, findlib, cstruct, ocaml_lwt, libev }:
stdenv.mkDerivation {
	name = "pcap-format";
	src = fetchurl {
		url = "https://github.com/mirage/ocaml-pcap/archive/0.3.3.tar.gz";
		sha256 = "0xmfssjy5fs9zk4scj03vb4b436bwak0blz31c9xkchh2dqa26cl";
	};
	buildInputs = [ocaml findlib cstruct ocaml_lwt libev ];
	propagateBuildInputs = [ libev ];
	createFindlibDestdir = true;
}

