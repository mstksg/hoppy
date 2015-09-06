{ mkDerivation, base, cppop, stdenv }:
mkDerivation {
  pname = "cppop-example-templates-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cppop ];
  license = stdenv.lib.licenses.agpl3;
}
