{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, clock, containers
      , network, pqueue, stdenv, text
      }:
      mkDerivation {
        pname = "obiwan";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base binary bytestring clock containers network pqueue text
        ];
        homepage = "https://github.com/blitz/obiwan#readme";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  stackLts = import ./stackage-13.10.nix { inherit (nixpkgs) pkgs; };

  drv = variant (stackLts.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
