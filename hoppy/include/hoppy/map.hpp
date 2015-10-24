#ifndef HOPPY_MAP_HPP
#define HOPPY_MAP_HPP

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

#include <map>

namespace hoppy {
namespace map {

template <typename K, typename V>
const K& getIteratorKey(const typename std::map<K, V>::const_iterator& iterator) {
    return iterator->first;
}

template <typename K, typename V>
V& getIteratorValue(typename std::map<K, V>::iterator& iterator) {
    return iterator->second;
}

template <typename K, typename V>
const V& getIteratorValue(const typename std::map<K, V>::const_iterator& iterator) {
    return iterator->second;
}

}  // namespace map
}  // namespace hoppy

#endif
