{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs
, pkgs ? import nixpkgs (import sources.haskell) }:
let
  niv = import sources.niv {};
in
pkgs.mkShell {
  nativeBuildInputs = [ niv.niv pkgs.stack pkgs.haskell-nix.nix-tools ];
}
