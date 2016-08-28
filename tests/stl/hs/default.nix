# This file is part of Hoppy.
#
# Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

{ mkDerivation, base, HUnit, stdenv
, hoppy-runtime, hoppy-tests-stl-cpp, hoppy-tests-stl-generator
}:
mkDerivation {
  pname = "hoppy-tests-stl";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [ base hoppy-runtime ];
  librarySystemDepends = [ hoppy-tests-stl-cpp ];
  testHaskellDepends = [ base hoppy-runtime HUnit ];
  license = stdenv.lib.licenses.agpl3Plus;
  doCheck = true;
  doHaddock = false;

  prePatch = ''
    ${hoppy-tests-stl-generator}/bin/generator --gen-hs .
  '';
}
