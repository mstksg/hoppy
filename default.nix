{ mkDerivation, base, binary, bytestring, containers, directory
, filepath, haskell-src, mtl, stdenv, text, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "cppop";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers directory filepath haskell-src
    mtl text
  ];
  homepage = "http://khumba.net/projects/cppop";
  description = "C++ FFI generator for Haskell";
  license = stdenv.lib.licenses.agpl3;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
