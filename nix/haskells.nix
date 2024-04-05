# This file is part of Hoppy.
#
# Copyright 2015-2024 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Evaluates to a set of Haskell package sets that should be used for building
# and testing.  Each is named either "ghcXXX" or "latest".

{ ... }@nixpkgsArgs:
with import ./nixpkgs.nix nixpkgsArgs;
let
  # We filter out old releases of each GHC X.Y series here because there are now
  # a lot of micro versions in Nixpkgs.
  blacklistedHaskells = [
    # ghc844 on Nixpkgs unstable channel (2020-03-29) fails to build haskell-src:
    #
    #   Warning: haskell-src.cabal:0:0: Unsupported cabal-version. See
    #   https://github.com/haskell/cabal/issues/4899.
    #   CallStack (from HasCallStack):
    #     die', called at libraries/Cabal/Cabal/Distribution/PackageDescription/Parsec.hs:110:13
    #         in Cabal-2.2.0.1:Distribution.PackageDescription.Parsec
    #     <snip>
    #   Setup: Failed parsing "./haskell-src.cabal".
    "ghc844"

    # Old releases of GHC 8.10.
    "ghc810"

    # syb is broken here as of 2021-01-25.
    "ghc901"

    # Old releases of GHC 9.2.
    "ghc924"
    "ghc925"
    "ghc926"
    "ghc927"

    # Old releases of GHC 9.4.
    "ghc942"
    "ghc943"
    "ghc944"
    "ghc945"
    "ghc946"
    "ghc947"

    # Old releases of GHC 9.6.
    "ghc962"
  ];

  isBlacklisted = name:
    # Two-digit versions are duplicates of the latest three-digit version.
    builtins.match "ghc[0-9][0-9]" name != null
    # Explicitly blacklisted versions.
    || builtins.elem name blacklistedHaskells;

  # Build against explicit GHC versions.  Build against all available GHC
  # versions by matching against /^ghc[0-9]+$/.  We explicitly don't want
  # ghc*Binary, ghcHEAD, ghcjs, and anything in the blacklist above.
  versionedHaskells =
    lib.filterAttrs
      (name: pkg: builtins.match "ghc[0-9]+" name != null && !(isBlacklisted name))
      haskell.packages;

  # If the latest GHC version (as provided by haskellPackages) isn't in the
  # above list, then include it explicitly.
  haskells =
    if builtins.elem (haskellPackages.ghc.name)
                     (map (hpkgs: hpkgs.ghc.name)
                          (builtins.attrValues versionedHaskells))
    then versionedHaskells
    else versionedHaskells // { latest = haskellPackages; };

in haskells
