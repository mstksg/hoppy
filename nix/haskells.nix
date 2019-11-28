# This file is part of Hoppy.
#
# Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
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

  # Build against explicit GHC versions.  Build against all available GHC
  # versions by matching against /^ghc[0-9]+$/.  We explicitly don't want
  # ghc*Binary, ghcHEAD, ghcjs.
  versionedHaskells =
    lib.filterAttrs (name: pkg: builtins.match "ghc[0-9]+" name != null)
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
