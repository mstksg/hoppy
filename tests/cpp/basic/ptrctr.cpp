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

#include "ptrctr.hpp"

int PtrCtr::constructions_ = 0;

int PtrCtr::destructions_ = 0;

PtrCtr::PtrCtr() {
    ++constructions_;
}

PtrCtr::PtrCtr(const PtrCtr&) {
    ++constructions_;
}

PtrCtr::PtrCtr(PtrCtr&&) {
    ++constructions_;
}

PtrCtr::~PtrCtr() {
    ++destructions_;
}

PtrCtr PtrCtr::newGcedObj() {
    return PtrCtr();
}

const PtrCtr& PtrCtr::newGcedRefConst() {
    return *new PtrCtr();
}

PtrCtr& PtrCtr::newGcedRef() {
    return *new PtrCtr();
}

const PtrCtr* PtrCtr::newGcedPtrConst() {
    return new PtrCtr();
}

PtrCtr* PtrCtr::newGcedPtr() {
    return new PtrCtr();
}

int PtrCtr::resetCounters() {
    constructions_ = 0;
    destructions_ = 0;
}

int PtrCtr::getConstructionCount() {
    return constructions_;
}

int PtrCtr::getDestructionCount() {
    return destructions_;
}

void PtrCtr::redButton() const {}
