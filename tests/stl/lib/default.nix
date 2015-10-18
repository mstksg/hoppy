# This file is part of Hoppy.
#
# Copyright 2015 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License version 3
# as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

{ stdenv, hoppy, hoppy-tests-stl-generator }:

let gen = hoppy-tests-stl-generator;
    name = "hoppy-tests-stl";
    libName = "lib${name}.so";
in

stdenv.mkDerivation {
  name = "${name}-lib-0.1.0";
  src = ./.;
  buildInputs = [ gen ];

  # Keep the -I... in sync with Makefile.
  prePatch = ''
    ${gen}/bin/generator --gen-cpp .
    substituteInPlace Makefile \
        --replace -I../../../hoppy/include "-I${hoppy}/include"
  '';

  installPhase = ''
    mkdir -p $out/src
    install -m 444 *.cpp *.hpp $out/src

    mkdir -p $out/lib
    install ${libName} $out/lib/${libName}.0.1.0
    cd $out/lib
    ln -s ${libName}.0.1{.0,}
    ln -s ${libName}.0{.1,}
    ln -s ${libName}{.0,}
  '';
}
