{stdenv, fetchurl, ocaml, findlib, base64, cmdliner, cstruct, ocaml_ipaddr, ocaml_lwt, mirage-profile, io-page, mirage-types, ounit, re, uri, pcap-format, libev }:
stdenv.mkDerivation {
	name = "ocaml_dns";
	src = fetchurl {
		url = "https://github.com/mirage/ocaml-dns/archive/v0.15.0.tar.gz";
		sha256 = "09r8x02x0waj4v2gynq3crgqkns2z3lnayapj38ss7xkrnb9n8m5";
	};
	configurePhase = "ocaml setup.ml -configure --enable-lwt --enable-mirage";
	# configureScript = "ocaml setup.ml";
	# configureFlags = "--enable-lwt --enable-mirage";
	buildInputs = [ocaml findlib base64 cmdliner cstruct ocaml_ipaddr ocaml_lwt mirage-profile io-page mirage-types ounit re uri pcap-format libev];
	propagateBuildInputs = [ libev ];
	createFindlibDestdir = true;
}

