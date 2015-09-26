let pkgs = import ../pkgs.nix {}; in
pkgs.stdenv.mkDerivation {
  name = "cppop-tests-0.1.0";
  builder = ./builder.sh;
  buildInputs = let h = pkgs.haskellPackages; in [
    pkgs.cppop-tests-basic-lib h.cppop-tests-basic
    pkgs.cppop-tests-stl-lib h.cppop-tests-stl
  ];
}
