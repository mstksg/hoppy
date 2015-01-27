# Defensive.  Scripts should still set this themselves to start with, in case
# sourcing this file fails:
set -euo pipefail

msg() {
    echo "cppop >>> $*"
}

run() {
    echo "*** $*"
    "$@"
}

declare -r projectDir="$(dirname "$(realpath "$0")")"
declare -r buildDir="$projectDir/../build-cppop-Desktop-Debug"
