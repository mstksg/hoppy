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

{ mkDerivation, base, binary, bytestring, containers, directory
, filepath, haskell-src, mtl, stdenv, text, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "hoppy";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers directory filepath haskell-src
    mtl text
  ];
  homepage = "http://khumba.net/projects/hoppy";
  description = "C++ FFI generator for Haskell";
  license = stdenv.lib.licenses.agpl3;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;

  postInstall = ''
    cp -r include $out
  '';
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
