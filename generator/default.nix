# This file is part of Hoppy.
#
# Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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

{ mkDerivation, lib
, base, bytestring, containers, directory, filepath, haskell-src, mtl
, process, temporary, text
, forceParallelBuilding ? false
}:
mkDerivation {
  pname = "hoppy-generator";
  version = "0.9.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory filepath haskell-src mtl process
    temporary text
  ];
  homepage = "https://khumba.net/projects/hoppy";
  description = "C++ FFI generator - Code generator";
  license = lib.licenses.agpl3Plus;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
}
