{ pkgs ? import <nixpkgs> {} }:
pkgs.callPackage ./opam2nix.nix { src = ./local.tgz; }
