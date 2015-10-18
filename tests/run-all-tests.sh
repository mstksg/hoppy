#!/bin/sh

# This file is part of Hoppy.
#
# Copyright 2015 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License version 3
# as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Runs all of Hoppy's unit test suites.  Aborts if any one of them fails.

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
