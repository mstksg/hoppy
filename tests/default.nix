let pkgs = import ../pkgs.nix {}; in
pkgs.stdenv.mkDerivation {
  name = "cppop-tests-0.1.0";
  builder = ./builder.sh;
  buildInputs = let h = pkgs.haskellPackages; in [
    h.cppop-tests-basic
    h.cppop-tests-stl
  ];
}
