#!/bin/sh

# TODO Use nix-shell here to get dependencies.

set -eux

niv update
stack-to-nix --output . --stack-yaml stack.yaml
