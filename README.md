# opam2nix is experimental software

I'm hoping to make it stable and a future part of `nixpkgs`. But for now, it's just this code which might work sometimes, and will probably change a lot.

## Building:

    $ nix-build

(of course)

If you'd like to hack, use `nix-shell` and just run `gup`.

## Usage:

There are a few parts:

### Step 1: generate a set of `nix` package definitions based on an opam repository.

(One day, this may be done as part of `nixpkgs`. But this is not that day.)

    $ opam2nix repo --src ~/.opam/repo/ocaml.org --dest dest/nix --cache dest/cache '*@latest'

This traverses the repo, scans the packages you've selected, downloads sources that it hasn't cached, reads `opam` files for dependencies, and spits out a `.nix` file for each version of each package.

### Step 2: select exact versions of each dependency

The generated `.nix` files are pretty dumb - they know about mandatory dependencies, and they know about all possible optional dependencies. But they rely on you giving them a valid set of dependencies which satisfy all versioning constraints, conflicts, etc. Conveniently, this is exactly what `opam`'s solver does - but instead of actually installing everything, let's just get it to create a `nix` expression of the packages it _would_ install:

    $ opam2nix select --repo lwt --dest selection.nix

I'd recommend not modifying that file, so that you can regenerate it fearlessly.

You can use it like so:

    { pkgs ? import <nixpkgs> {}}:
    let
      selection = pkgs.callPackage ./selection.nix {
        # one day, both of these may be rolled into `nixpkgs`, making them optional:
        opam2nix = /path/to/opam2nix/default.nix;
        opamPackages = import ./dest/nix;
      };
    in
    {
      name = "foo-bar";
      buildInputs = [ selection.lwt ];
      # ...
    }

## Does it work?

A bit! Here are some things that aren't really covered yet:

 - different versions of `ocaml` (it'll just use `pkgs.ocaml`, and it can't even tell what version that is)
 - base packages corresponding to the selected ocaml version (I've hardcoded a few to get things working for now)

