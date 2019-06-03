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

# Like all-builds-set.nix, this file evaluates to a set of derivations that run
# Hoppy's test suite.  There is an attribute in this set, for each Haskell
# package set provided by haskells.nix, for each package provided by
# tests-set-1.nix.
#
# To build all of these packages: nix-build tests.nix
#
# To query packages in this set: nix-env -f tests-set.nix -qaP
#
# To build a specific package in this set: nix-build tests-set.nix -A <attrName>

{ ... }@nixpkgsArgs:
with import ./nixpkgs.nix nixpkgsArgs;
let

  # Load all of the Haskell package sets we'll build against.
  haskells = import ./haskells.nix nixpkgsArgs;

in

# Include all tests:
lib.foldl (x: y: x // y) {}
  (lib.mapAttrsToList
     (setName: hpkgs:
        let pkgs = import ./tests-set-1.nix hpkgs; in
        lib.mapAttrs'
          (pkgName: pkg: lib.nameValuePair "${setName}-${pkgName}" pkg)
          pkgs)
     haskells)

# Also explicitly include all main packages, to ensure that they build: (In
# particular, the docs aren't covered by unit tests.)
// import ./all-builds-set.nix nixpkgsArgs
