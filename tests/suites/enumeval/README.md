This test suite tests the logic around enum evaluation.

This suite also serves as a test that enum evaluation includes the sources
directory of the C++ gateway package on the header search path during its
compilation.  This is done by Hoppy automatically so that bindings don't have to
resort to custom compiler configuration or other workarounds to use evaluation
on enums declared within the C++ gateway package.

---

This file is part of Hoppy.

Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
