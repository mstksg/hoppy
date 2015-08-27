{ cabal, binary, filepath, haskellSrc, mtl, text
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:

cabal.mkDerivation (self: {
  pname = "cppop";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ binary filepath haskellSrc mtl text ];

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;

  meta = {
    homepage = "http://khumba.net/projects/cppop";
    description = "C++ FFI generator for Haskell";
    license = self.stdenv.lib.licenses.agpl3;
    platforms = self.ghc.meta.platforms;
  };
} // (if enableSplitObjs != null then { inherit enableSplitObjs; } else {}))
