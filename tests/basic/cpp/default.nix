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

{ stdenv, hoppy, hoppy-tests-basic-generator }:

let gen = hoppy-tests-basic-generator;
    name = "hoppy-tests-basic";
    libName = "lib${name}.so";
in

stdenv.mkDerivation {
  name = "${name}-cpp-0.2.0";
  src = ./.;
  buildInputs = [ gen ];

  prePatch = ''
    ${gen}/bin/generator --gen-cpp .
  '';

  CXXFLAGS = "-I${hoppy}/include";

  installPhase = ''
    mkdir -p $out/src
    install -m 444 *.cpp *.hpp $out/src

    mkdir -p $out/lib
    install ${libName} $out/lib/${libName}.0.2.0
    cd $out/lib
    ln -s ${libName}.0.1{.0,}
    ln -s ${libName}.0{.1,}
    ln -s ${libName}{.0,}
  '';

  meta = {
    license = stdenv.lib.licenses.agpl3Plus;
  };
}
