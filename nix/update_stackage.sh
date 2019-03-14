#!/usr/bin/env nix-shell
#!nix-shell -i bash -p stack2nix

if [ ! -f "../obiwan.cabal" ]; then
  echo "You are supposed to run this script from the obiwan project's nix folder"
  exit 1
fi

stack2nix .. > stackage.nix
