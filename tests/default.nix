let pkgs = import ../pkgs.nix {}; in
pkgs.stdenv.mkDerivation {
  name = "hoppy-tests-0.1.0";
  builder = ./builder.sh;
  buildInputs = let h = pkgs.haskellPackages; in [
    pkgs.hoppy-tests-basic-lib h.hoppy-tests-basic
    pkgs.hoppy-tests-circular-lib h.hoppy-tests-circular
    pkgs.hoppy-tests-stl-lib h.hoppy-tests-stl
  ];
}
