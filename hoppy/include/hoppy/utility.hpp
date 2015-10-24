#ifndef HOPPY_UTILITY_HPP
#define HOPPY_UTILITY_HPP

// This file is part of Hoppy.
//
// Copyright 2015 Bryan Gardiner <bog@khumba.net>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License version 3
// as published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <utility>

namespace hoppy {
namespace utility {

template <typename A, typename B>
A& pairFirst(std::pair<A, B>& pair) {
    return pair.first;
}

template <typename A, typename B>
const A& pairFirst(const std::pair<A, B>& pair) {
    return pair.first;
}

template <typename A, typename B>
B& pairSecond(std::pair<A, B>& pair) {
    return pair.second;
}

template <typename A, typename B>
const B& pairSecond(const std::pair<A, B>& pair) {
    return pair.second;
}

}  // namespace utility
}  // namespace hoppy

#endif
