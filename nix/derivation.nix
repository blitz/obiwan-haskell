{ mkDerivation, base, binary, bytestring, hslua, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "obiwan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring hslua network optparse-applicative text
  ];
  homepage = "https://github.com/blitz/obiwan#readme";
  license = stdenv.lib.licenses.agpl3;
}
