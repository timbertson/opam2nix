{ pkgs ? import <nixpkgs> {}}:
pkgs.callPackage ./nix/opam2nix.nix {}
