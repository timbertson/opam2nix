The examples show some concrete uses of `opam2nix`, and should be buildable as long as you have a recent `nixpkgs`.

# Scenario 1. [simple](./simple)

The simple use case is where all dependencies are in OPAM, you just want to build them via nix.

# Scenario 2. [opam-library](./opam-library)

If you're developing more complex software, you may need to integrate ocaml dependencies from outside the official opam repository.

This example uses the `vdoml` library, which is not in the opam repositories.

The main change is that the source for `vdoml` is required when building the initial selections document, as well as at build time. This is achieved by making `nix/default.nix` return an attribute set instead of a single derivation.

The `make` target which builds the `opam-selections.nix` document passes in the path to vdoml which it generated with `nix-build --attr vdoml`, and the final build runs on `nix-build --attr hello` (the demo program).

### Layout

Each example has:

 - default.nix: self-contained derivation which imports opam2nix and `<nixpkgs>` explicitly
 - opam-selection.nix: generated information for all opam packages in use
 - nix/default.nix: a parameterised derivation as you might find in `nixpkgs` - i.e. all dependencies injected. Uses the `opam2nix` API to build `opam-selection.nix`
 - Makefile: commands for building the actual derivation, and for (re)generating `opam-selection.nix`

The examples reference `./examples/opam2nix.nix`, which imports opam2nix directly from the parent directory. In your own repository, you would import it from github instead (see the toplevel README for instructions).
