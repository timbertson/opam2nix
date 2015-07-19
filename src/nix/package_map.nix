{pkgs, ocamlPackages}:
let
	nameMap = {
		lwt = "ocaml_lwt";
	};
in
rec {
	name = opamName: (if lib.hasAttr opamName nameMap then getattr opamName nameMap else name);
	pkg = opamName: getattr (name opamName) ocamlPackages;
}
