#ifndef HOPPY_INTBOX_HPP
#define HOPPY_INTBOX_HPP

// This file is part of Hoppy.
//
// Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

#include <functional>

class IntBox {
public:
    IntBox() : n_(0) {}
    IntBox(int n) : n_(n) {}
    virtual ~IntBox() {}

    int get() const { return n_; }
    void set(int n) { n_ = n; }

protected:
    int n_;
};

class IntBoxComparable : public IntBox {
public:
    IntBoxComparable() {}
    IntBoxComparable(int n) : IntBox(n) {}

    bool operator<(const IntBoxComparable& other) const { return n_ < other.n_; }
    bool operator<=(const IntBoxComparable& other) const { return n_ <= other.n_; }
    bool operator>(const IntBoxComparable& other) const { return n_ > other.n_; }
    bool operator>=(const IntBoxComparable& other) const { return n_ >= other.n_; }
};

class IntBoxEquatable : public IntBox {
public:
    IntBoxEquatable() {}
    IntBoxEquatable(int n) : IntBox(n) {}

    bool operator==(const IntBoxEquatable& other) const { return n_ == other.n_; }
    bool operator!=(const IntBoxEquatable& other) const { return !(*this == other); }
};

namespace std {

template<>
struct hash<IntBoxEquatable> : public std::unary_function<IntBoxEquatable, std::size_t> {
    std::size_t operator()(IntBox val) const { return std::hash<int>()(val.get()); }
};

}  // namespace std

#endif
