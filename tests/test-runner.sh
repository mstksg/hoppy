#!/bin/sh
#
# Driver script for a unit test suite.  Builds and runs the test program in the
# current directory.  Should be sourced by run-test.sh scripts in test
# directories.  Normally cleans output files after a successful test run.  Can
# be given the argument "clean" to only clean test outputs without building, and
# the environment variables "doBuild" and "doClean" can be set to "true" or
# "false" to control script behaviour (both default to true).

# Bash strict mode.
set -euo pipefail

if test "${1:-}" = clean; then
    doBuild=false
    doClean=true
fi

declare -r suiteRoot=$PWD
declare -r suite=$(basename "$suiteRoot")
declare -r doBuild=${doBuild:-true}
declare -r doClean=${doClean:-true}

# This variable may be set in individual run-test.sh files to specify additional
# build outputs to clean up.
declare -r filesToClean=${filesToClean:-}

if $doBuild; then
    echo "Running tests in $suite."

    # Build the generator and its dependencies.
    set -x
    cd "$suiteRoot/../../cppop"
    cabal build
    cd "$suiteRoot/generator"
    cabal build
    cd ../hs
    cabal sandbox delete
    cabal sandbox init
    cabal install ../../../cppop
    cabal install ../generator

    # Build the C++ library.
    cd ../lib
    make clean
    ../hs/.cabal-sandbox/bin/generator --gen-cpp .
    make
    cd ../hs

    # Build and run the Haskell test program.
    .cabal-sandbox/bin/generator --gen-hs .
    cabal configure --enable-tests --extra-lib-dirs="$suiteRoot/lib"
    LD_LIBRARY_PATH="$suiteRoot/lib" cabal test
    set +x
    cd ..
fi

# Optionally, and on success, clean up after ourselves.
if $doClean; then
    echo "Cleaning tests in $suite."
    cd hs
    set +e
    cabal sandbox delete
    cabal clean
    rm -fv Foreign/Cppop/Test/*.hs{,-boot}
    cd ../generator
    cabal clean
    cd ..
    rm -fv -- lib/"$suite".{hpp,cpp} lib/*.o lib/*.so $filesToClean
fi
