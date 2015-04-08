#!/usr/bin/env bash

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

echo
msg "Building the generator and Haskell bindings."
run cd "$projectDir/lang/hs"
run cabal configure
run cabal build
