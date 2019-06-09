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

{ mkDerivation, base, Cabal, containers, hoppy-runtime
, hoppy-tests-basic-cpp, hoppy-tests-generator, HUnit, stdenv
}:
mkDerivation {
  pname = "hoppy-tests-basic";
  version = "0.3.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal hoppy-runtime ];
  libraryHaskellDepends = [
    base hoppy-runtime hoppy-tests-basic-cpp hoppy-tests-generator
  ];
  # librarySystemDepends = [ hoppy-tests-basic ];
  testHaskellDepends = [ base containers hoppy-runtime HUnit ];
  doHaddock = false;
  license = stdenv.lib.licenses.agpl3Plus;

  enableSharedExecutables = true;

  # Tell the generator where the C++ files are for this package.
  preConfigure = ''
    export HOPPY_TEST_CPP_DIR="${hoppy-tests-basic-cpp}/include/hoppy-tests-basic-cpp"
  '';
}
