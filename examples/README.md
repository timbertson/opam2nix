The examples show some concrete uses of `opam2nix`, and should be buildable as long as you have a recent `nixpkgs`.

# Scenario 1. [simple](./simple)

The simple use case is where all dependencies are in OPAM, you just want to build them via nix.

# Scenario 2. [opam-library](./opam-library)

If you're developing more complex software, you may need to integrate ocaml dependencies from outside the official opam repository.

This example uses the `vdoml` library, which is not in the opam repositories.

### Layout

Each example has:

 - default.nix: self-contained derivation which imports opam2nix and `<nixpkgs>` explicitly
 - opam-selection.nix: generated information for all opam packages in use
 - nix/default.nix: a parameterised derivation as you might find in `nixpkgs` - i.e. all dependencies injected. Uses the `opam2nix` API to build `opam-selection.nix`
 - Makefile: commands for building the actual derivation, and for (re)generating `opam-selection.nix`

The examples reference `./examples/opam2nix.nix`, which imports opam2nix directly from the most recent github commit.
