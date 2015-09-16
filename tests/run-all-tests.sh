#!/bin/sh
#
# Runs all of Cppop's unit test suites.  Aborts if any one of them fails.

# Bash strict mode.
set -euo pipefail

# Go to this file's directory.
myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
cd "$myDir"

for testScript in */run-test.sh; do
    echo
    echo "**********"
    echo
    "$testScript"
done

echo
echo "********** Success!"
