{ stdenv, hoppy-example-templates-generator }:

let gen = hoppy-example-templates-generator;
    name = "hoppy-example-templates";
    libName = "lib${name}.so";
in

stdenv.mkDerivation {
  name = "${name}-lib-0.1.0";
  src = ./.;
  buildInputs = [ gen ];

  prePatch = ''
    ${gen}/bin/generator --gen-cpp .
  '';

  installPhase = ''
    mkdir -p $out/lib
    install ${libName} $out/lib/${libName}.0.1.0
    cd $out/lib
    ln -s ${libName}.0.1{.0,}
    ln -s ${libName}.0{.1,}
    ln -s ${libName}{.0,}
  '';
}
