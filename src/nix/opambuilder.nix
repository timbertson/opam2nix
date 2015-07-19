{ stdenv }:
{ drv }:
let recognisedExtras = drv.passthru.recognisedExtras;
# Each opam package lists "extras". By default we omit the extras,
# but if a parent package includes an extra which is regocnised by
# a child then we must create a different derivation to compile the
# child with that extra package present.
#
# This behaviour is required by a lot of meta packages, which depend on
# libMain + libExtra in order to get a flavour of libMain with support
# for libExtra.
#
# Extra packages which aren't recognised by a child are passed through
# to their children, but do not otherwise result in a different derivation.

stdenv.mkDerivation (lib.merge drv {
	passthru.withOpamOptionalPackages = names: {
		lib.overrideDerivation drv (base: {
		});
	};
})
