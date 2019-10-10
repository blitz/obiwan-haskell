#!/bin/sh

# TODO Use nix-shell here to get dependencies.

stack-to-nix --output . --stack-yaml stack.yaml

