#!/bin/sh

# TODO Use nix-shell here to get dependencies.

set -eux

nix-prefetch-git --quiet https://github.com/input-output-hk/haskell.nix > nix/haskell-nix-src.json
stack-to-nix --output . --stack-yaml stack.yaml
