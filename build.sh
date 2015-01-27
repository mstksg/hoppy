#!/usr/bin/env bash

# Accepts MAKEOPTS.

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

echo
msg "Building the server."
if ! [[ -d $buildDir ]]; then
    run mkdir "$buildDir"
fi
run cd "$buildDir"
run qmake "$projectDir/lang/cpp"
run make ${MAKEOPTS:-}

echo
msg "Building the generator and Haskell bindings."
run cd "$projectDir/lang/hs"
run cabal configure
run cabal build
