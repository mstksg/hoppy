#!/bin/sh
#
# Builds and runs the unit test suite in this directory.  Testing is done within
# a Cabal sandbox for isolation.

# Bash strict mode.
set -euo pipefail

# Go to this file's directory.
myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
cd "$myDir"

filesToClean="lib/flobm.?pp lib/flubm.?pp"

. ../test-runner.sh
