{ mkDerivation, stdenv, base, cppop, haskell-src }:
mkDerivation {
  pname = "cppop-tests-stl-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cppop haskell-src ];
  license = stdenv.lib.licenses.agpl3;
}
