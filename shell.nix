{ nixpkgs ? import ./nix/nixpkgs.nix, pkgs ? import nixpkgs { } }:
pkgs.mkShell { nativeBuildInputs = [ pkgs.niv pkgs.stack ]; }
