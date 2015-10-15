{ mkDerivation, base, hoppy, stdenv, HUnit
, hoppy-tests-circular-generator, hoppy-tests-circular-lib
}:

let gen = hoppy-tests-circular-generator; in

mkDerivation {
  pname = "hoppy-tests-circular";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base hoppy ];
  executableSystemDepends = [ hoppy-tests-circular-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
