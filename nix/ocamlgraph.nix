# fork of https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/ocaml-modules/ocamlgraph/default.nix
# but without lablgtk support
{stdenv, fetchurl, ocaml, findlib}:

stdenv.mkDerivation rec {
  name = "ocamlgraph-${version}";
  version = "1.8.8";

  src = fetchurl {
    url = "http://ocamlgraph.lri.fr/download/ocamlgraph-${version}.tar.gz";
    sha256 = "0m9g16wrrr86gw4fz2fazrh8nkqms0n863w7ndcvrmyafgxvxsnr";
  };

  buildInputs = [ ocaml findlib ];

  patches = ./ocamlgraph-destdir.patch;

  postPatch = ''
    sed -i 's@$(DESTDIR)$(OCAMLLIB)/ocamlgraph@$(DESTDIR)/lib/ocaml/${ocaml.version}/site-lib/ocamlgraph@' Makefile.in
    sed -i 's@OCAMLFINDDEST := -destdir $(DESTDIR)@@' Makefile.in
  '';

  createFindlibDestdir = true;

  buildPhase = ''
    make all
    make install-findlib
  '';
}
