{ mkDerivation, base, hoppy, stdenv
, hoppy-example-templates-generator, hoppy-example-templates-lib
}:

let gen = hoppy-example-templates-generator; in

mkDerivation {
  pname = "hoppy-example-templates";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hoppy ];
  executableSystemDepends = [ hoppy-example-templates-lib ];
  license = stdenv.lib.licenses.agpl3;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
