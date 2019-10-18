The examples show some concrete uses of `opam2nix`, and should be buildable as long as you have a recent `nixpkgs`.

# Scenario 1. [opam-dependencies](./opam-dependencies)

This is when your dependencies are in OPAM (and possibly nix), but you just want to build some software in nix. This is fairly straightforward, you can just use `opam2nix` to provide some dependencies in an otherwise standard nix derivation.

# Scenario 2. [opam-library](./opam-library)

This is when you are developing an ocaml library / application (i.e. an `.opam` file is sufficient to describe your software), and you want to use `opam2nix` to build it.

### Layout

Each example has:

 - default.nix: self-contained derivation which imports opam2nix and `<nixpkgs>` explicitly
 - opam-selection.nix: generated information for all opam packages in use
 - nix/default.nix: a parameterised derivation as you might find in `nixpkgs` - i.e. all dependencies injected. Uses the `opam2nix` API to build `opam-selection.nix`
 - Makefile: commands for building the actual derivation, and for (re)generating `opam-selection.nix`

The examples reference `./examples/opam2nix.nix`, which imports opam2nix directly from the most recent github commit.
