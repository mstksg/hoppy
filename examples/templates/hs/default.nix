# This file is part of Hoppy.
#
# Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

{ mkDerivation, base, hoppy, stdenv
, hoppy-example-templates-generator, hoppy-example-templates-lib
}:

let gen = hoppy-example-templates-generator; in

mkDerivation {
  pname = "hoppy-example-templates";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hoppy ];
  executableSystemDepends = [ hoppy-example-templates-lib ];
  license = stdenv.lib.licenses.agpl3Plus;

  prePatch = ''
    ${gen}/bin/generator --gen-hs .
  '';
}
