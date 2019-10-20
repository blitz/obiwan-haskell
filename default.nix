{ nixpkgs ? import ./nix/nixpkgs.nix, pkgs ? import nixpkgs { } }:
let
  obiwanComponents =
    (import ./nix/build.nix { inherit pkgs; }).obiwan.components;
in {
  obiwan = obiwanComponents.exes.obiwan;

  # This currently fails to build due to "ghc: could not execute: hspec-discover"
  #test = obiwanComponents.tests.obiwan-test;
}
