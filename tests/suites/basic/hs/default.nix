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

{ mkDerivation, base, Cabal, containers, hoppy-runtime
, hoppy-tests-basic-cpp, hoppy-tests-generator, HUnit, lib
}:
mkDerivation {
  pname = "hoppy-tests-basic";
  version = "0.1.0";
  src = ./.;
  setupHaskellDepends = [
    base Cabal hoppy-runtime hoppy-tests-generator
  ];
  libraryHaskellDepends = [
    base containers hoppy-runtime hoppy-tests-basic-cpp
  ];
  # librarySystemDepends = [ hoppy-tests-basic ];
  testHaskellDepends = [ base containers hoppy-runtime HUnit ];
  doHaddock = false;
  license = lib.licenses.agpl3Plus;

  enableSharedExecutables = true;
}
