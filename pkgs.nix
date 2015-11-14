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

# This is a modified Nixpkgs that contains all of the individual Hoppy packages.

{ haskellOptions ? if builtins.pathExists ./config.nix
                   then import ./config.nix
                   else {}
, compiler ? null
}:

let

  realCompiler = if compiler == null then "ghc7102" else compiler;

  hoppyDir = ./.;

  haskellOverrides = haskellOptions: {
    overrides = self: super: {
      hoppy = self.callPackage (hoppyDir + /hoppy) haskellOptions;

      hoppy-example-templates-generator =
        self.callPackage (hoppyDir + /examples/templates/generator) {};
      hoppy-example-templates =
        self.callPackage (hoppyDir + /examples/templates/hs) {};

      hoppy-tests-basic-generator =
        self.callPackage (hoppyDir + /tests/basic/generator) {};
      hoppy-tests-basic =
        self.callPackage (hoppyDir + /tests/basic/hs) {};

      hoppy-tests-circular-generator =
        self.callPackage (hoppyDir + /tests/circular/generator) {};
      hoppy-tests-circular =
        self.callPackage (hoppyDir + /tests/circular/hs) {};

      hoppy-tests-stl-generator =
        self.callPackage (hoppyDir + /tests/stl/generator) {};
      hoppy-tests-stl =
        self.callPackage (hoppyDir + /tests/stl/hs) {};
    };
  };

  packageOverrides = pkgs: rec {
    hoppy-example-templates-lib = pkgs.callPackage (hoppyDir + /examples/templates/lib) {
      inherit (haskellPackages) hoppy-example-templates-generator;
    };

    hoppy-tests-basic-lib = pkgs.callPackage (hoppyDir + /tests/basic/lib) {
      inherit (haskellPackages) hoppy hoppy-tests-basic-generator;
    };

    hoppy-tests-circular-lib = pkgs.callPackage (hoppyDir + /tests/circular/lib) {
      inherit (haskellPackages) hoppy hoppy-tests-circular-generator;
    };

    hoppy-tests-stl-lib = pkgs.callPackage (hoppyDir + /tests/stl/lib) {
      inherit (haskellPackages) hoppy hoppy-tests-stl-generator;
    };

    haskellPackages =
      pkgs.haskell.packages.${realCompiler}.override (haskellOverrides haskellOptions);
  };

in import <nixpkgs> { config = { inherit packageOverrides; }; }
