#!/usr/bin/env bash

# This file is part of Hoppy.
#
# Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Builds and installs Hoppy.

set -euo pipefail

if [[ $# -ne 0 ]]; then
    cat <<EOF
install.sh - Hoppy build script

Builds and installs Hoppy into the local Cabal database.
EOF

    if [[ $* = --help ]]; then
        exit 0
    else
        exit 1
    fi
fi

projectDir=$(readlink -f "$0")
projectDir=$(dirname "$projectDir")
set -x

cd "$projectDir/generator"
cabal configure --ghc-options=-Werror --enable-tests
cabal build
cabal test
cabal haddock
cabal install --force-reinstalls

cd "$projectDir/std"
cabal configure --ghc-options=-Werror
cabal build
cabal haddock
cabal install --force-reinstalls

cd "$projectDir/runtime"
cabal configure --ghc-options=-Werror
cabal build
cabal haddock
cabal install --force-reinstalls

cd "$projectDir/docs"
cabal configure --ghc-options=-Werror
cabal build
cabal haddock
cabal install --force-reinstalls
