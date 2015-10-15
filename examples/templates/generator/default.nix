{ mkDerivation, base, hoppy, stdenv }:
mkDerivation {
  pname = "hoppy-example-templates-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hoppy ];
  license = stdenv.lib.licenses.agpl3;
}
