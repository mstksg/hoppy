# This file is part of Hoppy.
#
# Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

# This is a helper for building derivations simply include links to other
# derivations in the output, based on a provided set.
#
# The 'name' argument determines the name of the returned derivation.
#
# The 'pkgSet' argument should be a set whose values are derivations.  For each
# derivation here, the returned derivation will contain a symlink to the input
# derivation output path, named the same as the attribute in the set.
#
# After applying this file to its first argument set, that value should be
# passed to Nixpkgs's callPackage.

{ name, pkgSet }:
{ stdenv, lib, ... }:
let esc = lib.escapeShellArg; in
stdenv.mkDerivation {
  inherit name;

  buildInputs = builtins.attrValues pkgSet;

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out
  '' + lib.concatStringsSep "\n"
         (lib.mapAttrsToList
            (name: pkg: "ln -sT ${esc (builtins.toString pkg)} $out/${esc name}")
            pkgSet);
}
