#!/usr/bin/env bash

# This file is part of Hoppy.
#
# Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
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

# Driver for running the Hoppy unit tests.  Builds and runs all test suites, or
# just the ones provided on the command line.  Normally cleans output files
# after a successful test run.  The environment variables "doBuild" and
# "doClean" can be set to non-empty (true) or empty (false) to control script
# behaviour (both default to true).

# Bash strict mode.
set -euo pipefail

# Go to this file's directory.
myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
cd "$myDir"

declare -r testsRoot=$PWD

# The Foreign.Hoppy.Test.Interfaces.Compiler module expects this variable to
# point to the directory above each of the test suite's C++ files.
# FIXME: export HOPPY_TEST_SUITES_DIR="$testsRoot/suites"

if [[ $# -eq 0 ]]; then
    declare -r suites="basic circular stl"
else
    declare -r suites="$*"
fi

declare -r doBuild=${doBuild-y}
declare -r doClean=${doClean-y}

announce() {
    echo
    echo "*** $*"
    echo
}

run() {
    echo "> $*"
    "$@" |& sed -u 's/^/    /'
}

if [[ -n $doBuild ]]; then
    # Build the generator.  It includes all interfaces.
    announce "Building the generator."
    cd "$testsRoot/generator"
    run cabal v1-sandbox delete || true
    run cabal v1-sandbox init
    run cabal v1-install ../../{generator,std}
    run cabal v1-configure --ghc-options=-Werror
    run cabal v1-build
fi

for suite in $suites; do
    announce "Running test suite '$suite'."

    if [[ -n $doBuild ]]; then
        # Build the C++ half of the current test suite.
        cd "$testsRoot/cpp/$suite"
        run make clean
        run ../../generator/dist/build/generator/generator --interface "$suite" --gen-cpp .
        run make

        # Build and run the Haskell half of the current test suite.
        cd "$testsRoot/hs/$suite"
        run cabal v1-sandbox delete || true
        run cabal v1-sandbox init
        run cabal v1-install ../../../runtime
        run ../../generator/dist/build/generator/generator --interface "$suite" --gen-hs .
        run cabal v1-configure \
            --ghc-options=-Werror --enable-tests --extra-lib-dirs="$testsRoot/cpp/$suite"
        run cabal v1-build --ghc-options=-Werror
        LD_LIBRARY_PATH="$testsRoot/cpp/$suite" run cabal v1-test
    fi

    # Clean up the test suite's outputs.
    if [[ -n $doClean ]]; then
        cd "$testsRoot/hs/$suite"
        run cabal v1-clean || true
        run cabal v1-sandbox delete || true
        run rm -f Foreign/Hoppy/Test/*.hs{,-boot}

        cd "$testsRoot/cpp/$suite"
        make clean || true
    fi
done

if [[ -n $doClean ]]; then
    announce "Cleaning the generator."
    cd "$testsRoot/generator"
    cabal v1-clean
    cabal v1-sandbox delete
fi

