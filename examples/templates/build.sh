#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
set -x
ghc -o Generator Generator.hs
mkdir -p build-cpp
./Generator --gen-cpp build-cpp --gen-hs .
(cd build-cpp && g++ -c -Wall -Werror -fpic *.cpp)
g++ -shared -o build-cpp/libhoppyexample.so build-cpp/*.o
ghc -Lbuild-cpp -lhoppyexample -o Main Main.hs
