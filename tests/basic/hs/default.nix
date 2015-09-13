{ mkDerivation, base, cppop, stdenv, HUnit
, cppop-tests-basic-generator, cppop-tests-basic-lib
}:

let gen = cppop-tests-basic-generator; in

mkDerivation {
  pname = "cppop-tests-basic";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base cppop ];
  executableSystemDepends = [ cppop-tests-basic-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
