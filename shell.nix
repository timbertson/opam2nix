{ pkgs ? import <nixpkgs> {}}:
pkgs.lib.overrideDerivation (pkgs.callPackage ./default.nix {}) (base: {
	src = src/..;
	unpackCmd = "cp -a $src ./";
	# XXX hacky; might be fedora-only
	CURL_CA_BUNDLE = "/etc/pki/tls/certs/ca-bundle.crt";
})
