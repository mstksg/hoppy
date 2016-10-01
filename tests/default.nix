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

{ compiler ? null }:
let pkgs = import ../pkgs.nix { inherit compiler; }; in
pkgs.stdenv.mkDerivation {
  name = "hoppy-tests-0.2.1";
  builder = ./builder.sh;
  buildInputs = let h = pkgs.haskellPackages; in [
    pkgs.hoppy-tests-basic-cpp h.hoppy-tests-basic
    pkgs.hoppy-tests-circular-cpp h.hoppy-tests-circular
    pkgs.hoppy-tests-stl-cpp h.hoppy-tests-stl
  ];
  meta = {
    license = pkgs.stdenv.lib.licenses.agpl3Plus;
  };
}
