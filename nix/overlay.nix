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

# This file is a Nixpkgs overlay that adds in the (non-testing) Hoppy packages.

let

  haskellOptions =
    if builtins.pathExists ../config.nix
    then import ../config.nix
    else {};

  haskellOverrides = hself: hsuper: {
    hoppy-generator = hsuper.callPackage ../generator haskellOptions;
    hoppy-std = hsuper.callPackage ../std haskellOptions;
    hoppy-runtime = hsuper.callPackage ../runtime haskellOptions;
    hoppy-docs = hsuper.callPackage ../docs haskellOptions;
  };

in self: super: {
  haskell = super.haskell // {
    packageOverrides = hself: hsuper:
      (super.haskell.packageOverrides or (x: y: {})) hself super //
      haskellOverrides hself hsuper;
  };
}
