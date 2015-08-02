# opam2nix is experimental software

I'm hoping to make it stable and a future part of `nixpkgs`. But for now, it's just this code which might work sometimes, and will probably change a lot.

## Building:

    $ nix-build

(of course)

If you'd like to hack, use `nix-shell` and just run `gup`.

## Usage:

There are a few parts. One day, steps 1 and 2 may be done in `nixpkgs` proper. This is not that day.

### Step 1: generate a set of `nix` package definitions based on an opam repository.

(One day, this may be done as part of `nixpkgs`. But this is not that day.)

    $ opam2nix repo --src ~/.opam/repo/ocaml.org --dest <dest>/nix --cache <dest>/cache '*@latest'

This traverses the repo, scans the packages you've selected, downloads sources that it hasn't cached, reads `opam` files for dependencies, and spits out a `.nix` file for each version of each package.

### Step 2: Implement manual overrides

The above step generates "pure" package definitions based only on the information in the `opam` repository. But in order to integrate cleanly with `nixpkgs`, some generated packages need to be modified. This is implemented as a nix expression which wraps the generated packages. Currently, this lives in `src/nix/`. So:

    $ cp -a src/nix/* <dest>/nix/

### Step 3: select exact versions of each dependency

The generated `.nix` files are pretty dumb - they know about mandatory dependencies, and they know about all possible optional dependencies. But they rely on you giving them a valid set of dependencies which satisfy all versioning constraints, conflicts, etc. Conveniently, this is exactly what `opam`'s solver does - but instead of actually installing everything, let's just get it to create a `nix` expression of the packages it _would_ install:

    $ opam2nix select --repo lwt --dest selection.nix

I'd recommend not modifying that file, so that you can regenerate it fearlessly.

You can use it like so:

    { pkgs ? import <nixpkgs> {}}:
    let
      selection = pkgs.callPackage ./selection.nix {
        # one day, both of these may be rolled into `nixpkgs`, making them optional:
        opam2nix = /path/to/opam2nix/default.nix;
        opamPackages = import ./<dest>/nix;
      };
    in
    {
      name = "foo-bar";
      buildInputs = [ selection.lwt ];
      # ...
    }

## Does it work?

A bit!

