#ifndef HOPPY_STD_UNORDERED_MAP_HPP
#define HOPPY_STD_UNORDERED_MAP_HPP

// This file is part of Hoppy.
//
// Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <unordered_map>

namespace hoppy {
namespace unordered_map {

template <typename K, typename V>
const K& getIteratorKey(const typename std::unordered_map<K, V>::const_iterator& iterator) {
    return iterator->first;
}

template <typename K, typename V>
V& getIteratorValue(typename std::unordered_map<K, V>::iterator& iterator) {
    return iterator->second;
}

template <typename K, typename V>
const V& getIteratorValue(const typename std::unordered_map<K, V>::const_iterator& iterator) {
    return iterator->second;
}

}  // namespace unordered_map
}  // namespace hoppy

#endif
