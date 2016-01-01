#ifndef HOPPY_FUNCTIONS_HPP
#define HOPPY_FUNCTIONS_HPP

// This file is part of Hoppy.
//
// Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

#include "basic.hpp"
#include "intbox.hpp"
#include "ptrctr.hpp"

// Calculates pi (approximately).
int piapprox();

// Returns is argument multiplied by two.
long timesTwo(long);

// Tests for TObjToHeap.
PtrCtr givePtrCtrByValue();
void givePtrCtrByValueToCallback(GetPtrCtrByValueCallback);

// Tests for passing objects with conversions.
//
//// Creates a box with the given value.
IntBox makeBoxByValue(int);
IntBox& makeBoxByRef(int);
const IntBox& makeBoxByRefConst(int);
IntBox* makeBoxByPtr(int);
const IntBox* makeBoxByPtrConst(int);

//// Extracts the value from a box.
int getBoxValueByValue(IntBox);
int getBoxValueByRef(IntBox&);
int getBoxValueByRefConst(const IntBox&);
int getBoxValueByPtr(IntBox*);
int getBoxValueByPtrConst(const IntBox*);

//// Creates a box with the given int in it, calls the callback with the box, and
//// returns whatever the callback returns.
int getBoxValueByValueCallbackDriver(GetBoxValueByValueCallback, int);
int getBoxValueByRefCallbackDriver(GetBoxValueByRefCallback, int);
int getBoxValueByRefConstCallbackDriver(GetBoxValueByRefConstCallback, int);
int getBoxValueByPtrCallbackDriver(GetBoxValueByPtrCallback, int);
int getBoxValueByPtrConstCallbackDriver(GetBoxValueByPtrConstCallback, int);

//// Passes the int on to the callback.  Inspects the box returned from the
//// callback and returns its value.
int makeBoxByValueCallbackDriver(MakeBoxByValueCallback, int);
int makeBoxByRefCallbackDriver(MakeBoxByRefCallback, int);
int makeBoxByRefConstCallbackDriver(MakeBoxByRefConstCallback, int);
int makeBoxByPtrCallbackDriver(MakeBoxByPtrCallback, int);
int makeBoxByPtrConstCallbackDriver(MakeBoxByPtrConstCallback, int);

// Primitive type sizeof checks.
bool isTrue(bool x);
bool isFalse(bool x);
size_t sizeOfBool();
size_t sizeOfChar();
size_t sizeOfShort();
size_t sizeOfInt();
size_t sizeOfLong();
size_t sizeOfLLong();
size_t sizeOfFloat();
size_t sizeOfDouble();
size_t sizeOfPtrdiff();
size_t sizeOfSize();
size_t sizeOfSSize();

// Numeric type passing tests.
int doubleInt(int);
long doubleLong(long);
float doubleFloat(float);
double doubleDouble(double);
int8_t doubleInt8(int8_t);
int32_t doubleInt32(int32_t);
uint16_t doubleUInt16(uint16_t);
uint64_t doubleUInt64(uint64_t);

// Testing raw pointers.
bool* getBoolPtr();
int* getIntPtr();
int** getIntPtrPtr();
IntBox** getIntBoxPtrPtr();

#endif
