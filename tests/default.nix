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

{ compiler ? null }:
let pkgs = import ../pkgs.nix { inherit compiler; }; in
pkgs.stdenv.mkDerivation {
  name = "hoppy-tests-0.1.0";
  builder = ./builder.sh;
  buildInputs = let h = pkgs.haskellPackages; in [
    pkgs.hoppy-tests-basic-lib h.hoppy-tests-basic
    pkgs.hoppy-tests-circular-lib h.hoppy-tests-circular
    pkgs.hoppy-tests-stl-lib h.hoppy-tests-stl
  ];
}
