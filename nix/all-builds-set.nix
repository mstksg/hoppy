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

# This file evaluates to a set of Hoppy derivations built against different
# versions of GHC.  Building this file with nix-build ensures that Hoppy builds
# against various versions of GHC, including the latest (see haskells.nix).
#
# To build all of these packages, run "nix-build all-builds.nix".
#
# The attribute names are of the form:
#
#     ${compiler}-${hoppyPackage}
#
# Where 'compiler' is one of the Haskell package sets, e.g. "ghc864" for
# haskell.packages.ghc864, and 'hoppyPackage' is any of the Hoppy packages,
# e.g. "hoppy-generator".
#
# If the latest stable version of GHC shipped by Nixpkgs happens not to be
# explicitly listed below, then it will be included with 'compiler' set to
# "latest" rather than a numbered "ghcXXX" string.

{ ... }@nixpkgsArgs:
with import ./nixpkgs.nix nixpkgsArgs;
let

  # Load all of the Haskell package sets we'll build against.
  haskells = import ./haskells.nix nixpkgsArgs;

  # The Hoppy packages we want to build.
  packageNames = [ "hoppy-generator" "hoppy-std" "hoppy-runtime" "hoppy-docs" ];

in

lib.foldl (x: y: x // y) {}
  (lib.mapAttrsToList
     (setName: hpkgs:
        builtins.listToAttrs
        (map (pkgName: lib.nameValuePair "${setName}-${pkgName}" hpkgs.${pkgName})
             packageNames))
     haskells)
