{ mkDerivation, base, cppop, stdenv }:
mkDerivation {
  pname = "cppop-tests-basic-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cppop ];
  license = stdenv.lib.licenses.agpl3;
}
