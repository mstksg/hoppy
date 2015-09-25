{ mkDerivation, base, cppop, stdenv, HUnit
, cppop-tests-stl-generator, cppop-tests-stl-lib
}:

let gen = cppop-tests-stl-generator; in

mkDerivation {
  pname = "cppop-tests-stl";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base cppop ];
  executableSystemDepends = [ cppop-tests-stl-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
