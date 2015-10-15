{ mkDerivation, base, hoppy, stdenv, HUnit
, hoppy-tests-stl-generator, hoppy-tests-stl-lib
}:

let gen = hoppy-tests-stl-generator; in

mkDerivation {
  pname = "hoppy-tests-stl";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base hoppy ];
  executableSystemDepends = [ hoppy-tests-stl-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
