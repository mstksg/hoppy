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

# A nix-build-able file whose output contains links to builds of all main Hoppy
# packages (i.e. not tests or examples), for the current version of GHC.

{ ... }@nixpkgsArgs:
with import ./nixpkgs.nix nixpkgsArgs;
callPackage (import ./set-to-links.nix {
  name = "all-builds";
  pkgSet = import ./all-builds-set.nix nixpkgsArgs;
}) {}
