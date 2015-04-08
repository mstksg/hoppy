#!/usr/bin/env bash

set -euo pipefail
. "$(dirname "$(realpath "$0")")/common.sh"

echo
msg "Installing the generator and Haskell bindings."
run cabal install "$projectDir/lang/hs"
