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

# Takes a Haskell package set (haskellPackages or haskell.packages.ghcXXX) from
# Nixpkgs for a specific GHC version, and returns a set containing derivations
# to build to run Hoppy unit tests.

haskellPackages:
with haskellPackages;
rec {
  # Build and run unit tests:

  hoppy-tests-generator = callPackage ../tests/generator {};

  hoppy-tests-basic-cpp = callPackage ../tests/suites/basic/cpp
    { inherit hoppy-tests-generator; };

  hoppy-tests-basic = callPackage ../tests/suites/basic/hs
    { inherit hoppy-tests-generator hoppy-tests-basic-cpp; };

  hoppy-tests-circular-cpp = callPackage ../tests/suites/circular/cpp
    { inherit hoppy-tests-generator; };

  hoppy-tests-circular = callPackage ../tests/suites/circular/hs
    { inherit hoppy-tests-generator hoppy-tests-circular-cpp; };

  hoppy-tests-stl-cpp = callPackage ../tests/suites/stl/cpp
    { inherit hoppy-tests-generator; };

  hoppy-tests-stl = callPackage ../tests/suites/stl/hs
    { inherit hoppy-tests-generator hoppy-tests-stl-cpp; };

  # Build the Hoppy example package as part of the test suite:

  hoppy-example-generator = callPackage ../example/example-generator {};

  hoppy-example-cpp = callPackage ../example/example-cpp
    { inherit hoppy-example-generator; };

  hoppy-example = callPackage ../example/example
    { inherit hoppy-example-generator hoppy-example-cpp; };
}
