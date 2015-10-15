{ mkDerivation, base, hoppy, stdenv, HUnit
, hoppy-tests-basic-generator, hoppy-tests-basic-lib
}:

let gen = hoppy-tests-basic-generator; in

mkDerivation {
  pname = "hoppy-tests-basic";
  version = "0.1.0";
  src = ./.;
  executableHaskellDepends = [ base hoppy ];
  executableSystemDepends = [ hoppy-tests-basic-lib ];
  testHaskellDepends = [ base HUnit ];
  license = stdenv.lib.licenses.agpl3;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
