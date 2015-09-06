{ mkDerivation, base, cppop, stdenv
, cppop-example-templates-generator, cppop-example-templates-lib
}:

let gen = cppop-example-templates-generator; in

mkDerivation {
  pname = "cppop-example-templates";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cppop ];
  executableSystemDepends = [ cppop-example-templates-lib ];
  license = stdenv.lib.licenses.agpl3;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
