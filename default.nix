{
  nixpkgs ? <nixpkgs>,
  pkgs ? import nixpkgs {},
  doBenchmark ? false
}:

let
  stackLts = import ./nix/stackage.nix { inherit pkgs; };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (stackLts.callPackage (import ./nix/derivation.nix) {});
in
  if pkgs.lib.inNixShell then drv.env else drv
