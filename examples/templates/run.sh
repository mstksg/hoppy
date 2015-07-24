#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
LD_LIBRARY_PATH=build-cpp ./Main
