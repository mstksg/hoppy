{ mkDerivation, stdenv, base, hoppy, haskell-src }:
mkDerivation {
  pname = "hoppy-tests-circular-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base haskell-src hoppy ];
  license = stdenv.lib.licenses.agpl3;
}
