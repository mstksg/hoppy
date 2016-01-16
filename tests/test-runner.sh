#!/bin/sh

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

# Driver script for a unit test suite.  Builds and runs the test program in the
# current directory.  Should be sourced by run-test.sh scripts in test
# directories.  Normally cleans output files after a successful test run.  Can
# be given the argument "clean" to only clean test outputs without building, and
# the environment variables "doBuild" and "doClean" can be set to non-empty
# (true) or empty (false) to control script behaviour (both default to true).

# Bash strict mode.
set -euo pipefail

if test "${1:-}" = clean; then
    doBuild=
    doClean=y
fi

declare -r suiteRoot=$PWD
declare -r suite=$(basename "$suiteRoot")
declare -r doBuild=${doBuild-y}
declare -r doClean=${doClean-y}

# This variable may be set in individual run-test.sh files to specify additional
# build outputs to clean up.
declare -r filesToClean=${filesToClean:-}

if test -n "$doBuild"; then
    echo "Running tests in $suite."

    # Build the generator and its dependencies.
    set -x
    cd "$suiteRoot/hs"
    cabal sandbox delete
    cabal sandbox init
    cabal install ../../../{generator,std,runtime}
    cabal install ../generator

    # Build the C++ library.
    cd ../cpp
    make clean
    ../hs/.cabal-sandbox/bin/generator --gen-cpp .
    make
    cd ../hs

    # Build and run the Haskell test program.
    .cabal-sandbox/bin/generator --gen-hs .
    cabal configure --enable-tests --extra-lib-dirs="$suiteRoot/cpp"
    LD_LIBRARY_PATH="$suiteRoot/cpp" cabal test
    set +x
    cd ..
fi

# Optionally, and on success, clean up after ourselves.
if test -n "$doClean"; then
    echo "Cleaning tests in $suite."
    cd hs
    set +e
    cabal sandbox delete
    cabal clean
    rm -fv Foreign/Hoppy/Test/*.hs{,-boot}
    cd ../generator
    cabal clean
    cd ..
    rm -fv -- cpp/"$suite".{hpp,cpp} cpp/*.o cpp/*.so $filesToClean
fi
