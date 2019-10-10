{ pkgs ? import <nixpkgs> {}}:

let
  # Import the Haskell.nix library,
  haskell = import ./haskell.nix { inherit pkgs; };

  # Import the file you will create in the stack-to-nix or cabal-to-nix step.
  my-pkgs = import ../pkgs.nix;

  # Stack projects use this:
  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = my-pkgs;
    pkg-def-extras = [
      # these extras will provide additional packages
      # ontop of the package set.  E.g. extra-deps
      # for stack packages. or local packages for
      # cabal.projects
    ];
    modules = [
      # specific package overrides would go here
      # example:
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
    ];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
