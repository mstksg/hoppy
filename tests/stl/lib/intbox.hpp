#ifndef HOPPY_INTBOX_HPP
#define HOPPY_INTBOX_HPP

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

class IntBox {
public:
    IntBox() : n_(0) {}
    IntBox(int n) : n_(n) {}
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

#endif
