# This file is part of Hoppy.
#
# Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

{ mkDerivation, base, haskell-src, hoppy-generator, hoppy-runtime, lib
, forceParallelBuilding ? false
}:
mkDerivation {
  pname = "hoppy-docs";
  version = "0.8.1";
  src = ./.;
  libraryHaskellDepends = [
    base haskell-src hoppy-generator hoppy-runtime
  ];
  homepage = "http://khumba.net/projects/hoppy";
  description = "C++ FFI generator - Documentation";
  license = lib.licenses.agpl3Plus;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
}
