#!/usr/bin/env bash

# This file is part of Hoppy.
#
# Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Builds and installs the Hoppy example project.

set -euo pipefail

if [[ $# -ne 0 ]]; then
    cat <<EOF
clean.sh - Hoppy example build script

Cleans build outputs of the Hoppy example project.
EOF

    if [[ $* = --help ]]; then
        exit 0
    else
        exit 1
    fi
fi

myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
set -x

cd "$myDir/example-generator"
cabal clean

cd "$myDir/example-cpp"
cabal clean

cd "$myDir/example"
cabal clean
