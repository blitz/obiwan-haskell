let obiwanComponents = (import ./nix/build.nix { }).obiwan.components;
in {
  obiwan = obiwanComponents.exes.obiwan;

  # This currently fails to build due to "ghc: could not execute: hspec-discover"
  #test = obiwanComponents.tests.obiwan-test;
}
