{ mkDerivation, base, cppop, stdenv, HUnit
, cppop-tests-circular-generator, cppop-tests-circular-lib
}:

let gen = cppop-tests-circular-generator; in

mkDerivation {
  pname = "cppop-tests-circular";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base cppop ];
  executableSystemDepends = [ cppop-tests-circular-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
