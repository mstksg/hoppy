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

# This file evaluates to a set of Hoppy derivations built against different
# versions of GHC.  Building this file with nix-build ensures that Hoppy builds
# against various versions of GHC, including the latest (see haskells.nix).
#
# To build all of these packages: nix-build all-builds-full.nix
#
# To query packages in this set: nix-env -f all-builds-set-full.nix -qaP
#
# To build a specific package in this set: nix-build all-builds-set-full.nix -A <attrName>
#
# The attribute names are just the individual Hoppy package names:
#
#     ${hoppyPackage}

{ ... }@nixpkgsArgs:
with import ./nixpkgs.nix nixpkgsArgs;
let

  # Load all of the Haskell package sets we'll build against.
  haskells = import ./haskells.nix nixpkgsArgs;

  # The Hoppy packages we want to build.
  packageNames = [ "hoppy-generator" "hoppy-std" "hoppy-runtime" "hoppy-docs" ];

  prefixName = prefix: drv: drv.overrideAttrs (oldAttrs:
    (x: if x ? name then x // { name = prefix + x.name; } else x)
      ((x: if x ? pname then x // { pname = prefix + x.pname; } else x)
        oldAttrs)
  );

in

lib.foldl (x: y: x // y) {}
  (lib.mapAttrsToList
     (setName: hpkgs:
        let prefix = "${setName}-";
        in builtins.listToAttrs
            (map (pkgName:
                   lib.nameValuePair "${prefix}${pkgName}"
                     (prefixName prefix hpkgs.${pkgName}))
                 packageNames))
     haskells)
