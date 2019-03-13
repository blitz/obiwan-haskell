#!/usr/bin/env nix-shell
#!nix-shell -i bash -p cabal2nix stack2nix

if [ ! -f "nix/stackage.nix" ]; then
  echo "You are supposed to run this script from the obiwan project's root"
  exit 1
fi

stack2nix . > nix/stackage.nix
cabal2nix . > nix/derivation.nix
