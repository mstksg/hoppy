{ stdenv, cppop, cppop-tests-circular-generator }:

let gen = cppop-tests-circular-generator;
    name = "cppop-tests-circular";
    libName = "lib${name}.so";
in

stdenv.mkDerivation {
  name = "${name}-lib-0.1.0";
  src = ./.;
  buildInputs = [ gen ];

  # Keep the -I... in sync with Makefile.
  prePatch = ''
    ${gen}/bin/generator --gen-cpp .
    substituteInPlace Makefile \
        --replace -I../../../cppop/include "-I${cppop}/include"
  '';

  installPhase = ''
    mkdir -p $out/src
    install -m 444 *.cpp *.hpp $out/src

    mkdir -p $out/lib
    install ${libName} $out/lib/${libName}.0.1.0
    cd $out/lib
    ln -s ${libName}.0.1{.0,}
    ln -s ${libName}.0{.1,}
    ln -s ${libName}{.0,}
  '';
}
