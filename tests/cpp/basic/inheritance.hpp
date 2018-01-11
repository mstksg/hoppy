#ifndef HOPPY_INHERITANCE_HPP
#define HOPPY_INHERITANCE_HPP

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

class InheritanceA {
public:
    virtual int aFoo() const { return 1; }
    virtual int aBar() const { return 2; }
};

class InheritanceB {
public:
    virtual int bFoo() const = 0;
};

class InheritanceC : public InheritanceA, public InheritanceB {
public:
    virtual int aBar() const { return 20; }
    virtual int bFoo() const { return 30; }
};

#endif
