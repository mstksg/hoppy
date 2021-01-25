#ifndef HOPPY_ENUMS_HPP
#define HOPPY_ENUMS_HPP

// This file is part of Hoppy.
//
// Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// This is a plain old unscoped enum for testing.
enum IceCream {
    CHOCOLATE = 1,
    BUBBLEGUM,
    BIRTHDAY_CAKE,
};

// This enum is about testing word splitting behaviour, and also testing a
// scoped enum.
enum class Number {
    one = 0,
    oneAndAHalf,
    Two,
    THREE,
    four_fiveSixSEVEN,
};

// Returns a ranking of how good the ice cream is.
int rankIceCream(IceCream);

#endif
