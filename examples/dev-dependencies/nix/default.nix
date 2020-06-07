# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
	args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_08) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
	opam-selection = opam2nix.build args;

	# We have two dev dependencies, of slightly different kinds:
	#
	# `utop` is declared as an optional dependency in `hello.opam`,
	# which means it'll be used only if it's already available. By
	# adding it to `resolve` arguments, we ensure it's available.
	#
	# `ocamlformat` is not mentioned at all in `hello.opam`.
	# So we add it to `resolve` arguments, as above.
	# But our `hello` package reference it, because it wasn't
	# listed as a dependency. So we also override the resulting
	# nix derivation to inject `ocamlFormat`.
	resolve = opam2nix.resolve args [
		"hello.opam" "utop" "ocamlformat"
	];

	# NOTE: If you only want `ocamlformat` to be available inside a nix shell
	# (and not `nix-build`), you should override the `hello` derivation
	# in shell.nix instead of here.
	hello = opam-selection.hello.overrideAttrs (super: {
		buildInputs = (super.buildInputs or []) ++ [opam-selection.ocamlformat];
	});
in
{
	inherit hello;
	inherit resolve;
}
