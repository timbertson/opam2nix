{stdenv, fetchurl, ocaml, findlib, cstruct, ocaml_ipaddr, ocaml_lwt, mirage-types, mirage-clock-unix, mirage-net-unix, mirage-profile, mirage-unix, io-page, mirage-console, pkgconfig, mirage-xen }:
stdenv.mkDerivation {
	name = "tcpip";
	src = fetchurl {
		url = "https://github.com/mirage/mirage-tcpip/archive/v2.4.3.tar.gz";
		sha256 = "1jzxl9cbp9z5k718y08rcvirbpx2il0kx2qql1ivy53h384i35bk";
	};
	postPatch=''
		sed -i -e 's|env PKG_CONFIG_PATH=`opam config var prefix`/lib/pkgconfig||' postconf.ml
	'';
	configureFlags = "--enable-xen";
	buildInputs = [ ocaml findlib cstruct ocaml_ipaddr ocaml_lwt mirage-types mirage-clock-unix mirage-net-unix mirage-profile mirage-unix io-page mirage-console pkgconfig mirage-xen ];
	createFindlibDestdir = true;
}

