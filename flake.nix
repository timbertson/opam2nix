{
  description = "Generates nix expressions from opam files";

  inputs =
    {
      nixpkgs.url = "nixpkgs/release-21.11";
      opam = {
        url = "github:ocaml/opam";
        flake = false;
      };
      opam-0install-solver = {
        url = "github:ocaml-opam/opam-0install-solver";
        flake = false;
      };
      opam-file-format = {
        url = "github:ocaml/opam-file-format";
        flake = false;
      };
      spdx_licenses = {
        url = "github:kit-ty-kate/spdx_licenses";
        flake = false;
      };
      zeroinstall = {
        url = "github:0install/0install";
        flake = false;
      };
    };

  outputs = inputs@{ self, nixpkgs, opam, opam-0install-solver, opam-file-format
                   , spdx_licenses, zeroinstall }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      opam2nix = import nix/default.nix {
        self = ./.;
        inherit opam opam-0install-solver opam-file-format
          spdx_licenses zeroinstall;
        ocaml-ng = pkgs.ocaml-ng;
      };
    in {
      packages.${system} = {
        inherit opam2nix;
      };
      defaultPackage.${system} = opam2nix;
    };
}
