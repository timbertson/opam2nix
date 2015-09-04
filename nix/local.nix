{ pkgs ? import <nixpkgs> {} }:
pkgs.callPackage ./default.nix {} { src = ./local.tgz; }
