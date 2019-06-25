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

{ mkDerivation, base, Cabal, hoppy-runtime, hoppy-tests-generator, stdenv
}:
mkDerivation {
  pname = "hoppy-tests-stl-cpp";
  version = "0.1.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal hoppy-runtime ];
  libraryHaskellDepends = [
    base hoppy-runtime hoppy-tests-generator
  ];
  license = stdenv.lib.licenses.agpl3Plus;

  # Tell the generator where the C++ files are for this package.
  preConfigure = ''
    export HOPPY_TEST_CPP_DIR="$PWD/cpp"
  '';

  # Install the C++ sources so that the hs/ package can use them for its
  # compiler call.
  postInstall = ''
    mkdir -p $out/include/hoppy-tests-stl-cpp
    cp cpp/*.hpp $out/include/hoppy-tests-stl-cpp
  '';
}
