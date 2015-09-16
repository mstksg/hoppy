#!/bin/sh
#
# Driver script for a unit test suite.  Builds and runs the test program in the
# current directory.  Should be sourced by run-test.sh scripts in test
# directories.

# Bash strict mode.
set -euo pipefail

declare -r suiteRoot=$PWD
declare -r suite=$(basename "$suiteRoot")
declare -r doClean=true

echo "Running tests in $suite."

# Build the generator.
set -x
cd hs
cabal sandbox delete
cabal sandbox init
cabal install ../../../cppop
cabal install ../generator

# Build the C++ library.
cd ../lib
../hs/.cabal-sandbox/bin/generator --gen-cpp .
make
cd ../hs

# Build and run the Haskell test program.
.cabal-sandbox/bin/generator --gen-hs .
cabal configure --enable-tests --extra-lib-dirs="$suiteRoot/lib"
LD_LIBRARY_PATH="$suiteRoot/lib" cabal test
set +x

# Optionally, and on success, clean up after ourselves.
if $doClean; then
    cabal sandbox delete
    cabal clean
    rm Foreign/Cppop/Test/*.hs
    cd ../lib
    rm "$suite".{hpp,cpp} *.o *.so
    cd ../generator
    cabal clean
fi
