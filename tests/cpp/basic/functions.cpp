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

#include "exceptions.hpp"
#include "functions.hpp"

#include <cstddef>
#include <string>
#include "constants.hpp"

int piapprox() {
    return 4;
}

long timesTwo(long n) {
    return n * 2;
}

long takesLongFn(long (*fn)(long), long value) {
    return -fn(value);
}

int takesIntBoxFn(IntBox* (*fn)(IntBox*), int value) {
    IntBox box(value);
    IntBox *box2 = fn(&box);
    int value2 = box2->get() * 5;
    delete box2;
    return value2;
}

PtrCtr givePtrCtrByValue() {
    return PtrCtr();
}

void givePtrCtrByValueToCallback(GetPtrCtrByValueCallback cb) {
    cb(PtrCtr());
}

IntBox makeBoxByValue(int value) {
    return IntBox(value);
}

IntBox& makeBoxByRef(int value) {
    return *new IntBox(value);
}

const IntBox& makeBoxByRefConst(int value) {
    return *new IntBox(value);
}

IntBox* makeBoxByPtr(int value) {
    return new IntBox(value);
}

const IntBox* makeBoxByPtrConst(int value) {
    return new IntBox(value);
}

int getBoxValueByValue(IntBox box) {
    return box.get();
}

int getBoxValueByRef(IntBox& box) {
    return box.get();
}

int getBoxValueByRefConst(const IntBox& box) {
    return box.get();
}

int getBoxValueByPtr(IntBox* box) {
    return box->get();
}

int getBoxValueByPtrConst(const IntBox* box) {
    return box->get();
}

int getBoxValueByValueCallbackDriver(GetBoxValueByValueCallback cb, int value) {
    return cb(IntBox(value));
}

int getBoxValueByRefCallbackDriver(GetBoxValueByRefCallback cb, int value) {
    IntBox box(value);
    return cb(box);
}

int getBoxValueByRefConstCallbackDriver(GetBoxValueByRefConstCallback cb, int value) {
    IntBox box(value);
    return cb(const_cast<const IntBox&>(box));
}

int getBoxValueByPtrCallbackDriver(GetBoxValueByPtrCallback cb, int value) {
    IntBox box(value);
    return cb(&box);
}

int getBoxValueByPtrConstCallbackDriver(GetBoxValueByPtrConstCallback cb, int value) {
    IntBox box(value);
    return cb(const_cast<const IntBox*>(&box));
}

int makeBoxByValueCallbackDriver(MakeBoxByValueCallback cb, int value) {
    return cb(value).get();
}

int makeBoxByRefCallbackDriver(MakeBoxByRefCallback cb, int value) {
    IntBox& box = cb(value);
    int result = box.get();
    delete &box;
    return result;
}

int makeBoxByRefConstCallbackDriver(MakeBoxByRefConstCallback cb, int value) {
    const IntBox& box = cb(value);
    int result = box.get();
    delete &box;
    return result;
}

int makeBoxByPtrCallbackDriver(MakeBoxByPtrCallback cb, int value) {
    IntBox* box = cb(value);
    int result = box->get();
    delete box;
    return result;
}

int makeBoxByPtrConstCallbackDriver(MakeBoxByPtrConstCallback cb, int value) {
    const IntBox* box = cb(value);
    int result = box->get();
    delete box;
    return result;
}

IntBox makeBoxToGc(int value) {
    return IntBox(value);
}

int callBoxToGcCallback(TakesBoxToGcCallback cb, int value) {
    return cb(IntBox(value));
}

bool isTrue(bool x) {
    // Use external linkage to prevent this from being reduced to the identity
    // function.
    return x ? constantlyTrue : constantlyFalse;
}

bool isFalse(bool x) {
    return x ? constantlyFalse : constantlyTrue;
}

size_t sizeOfBool() {
    return sizeof(bool);
}

size_t sizeOfChar() {
    return sizeof(char);
}

size_t sizeOfShort() {
    return sizeof(short);
}

size_t sizeOfInt() {
    return sizeof(int);
}

size_t sizeOfLong() {
    return sizeof(long);
}

size_t sizeOfLLong() {
    return sizeof(long long);
}

size_t sizeOfFloat() {
    return sizeof(float);
}

size_t sizeOfDouble() {
    return sizeof(double);
}

size_t sizeOfPtrdiff() {
    return sizeof(ptrdiff_t);
}

size_t sizeOfSize() {
    return sizeof(size_t);
}

size_t sizeOfSSize() {
    return sizeof(ssize_t);
}

int doubleInt(int x) {
    return x * 2;
}

long doubleLong(long x) {
    return x * 2;
}

float doubleFloat(float x) {
    return x * 2;
}

double doubleDouble(double x) {
    return x * 2;
}

int8_t doubleInt8(int8_t x) {
    return x * 2;
}

int32_t doubleInt32(int32_t x) {
    return x * 2;
}

uint16_t doubleUInt16(uint16_t x) {
    return x * 2;
}

uint64_t doubleUInt64(uint64_t x) {
    return x * 2;
}


bool* getBoolPtr() {
    static bool data = false;
    return &data;
}

int* getIntPtr() {
    static int data = 23;
    return &data;
}

int** getIntPtrPtr() {
    static int data = 1234;
    static int* ptr = &data;
    return &ptr;
}

IntBox** getIntBoxPtrPtr() {
    static IntBox box(1010);
    static IntBox* ptr = &box;
    return &ptr;
}

void doubleIntBoxPtrPtr(IntBox** p) {
    IntBox& box = **p;
    box.set(box.get() * 2);
}

void doubleIntPtr(int* p) {
    *p *= 2;
}

void doubleIntPtrPtr(int** p) {
    **p *= 2;
}

void doubleIntRef(int& n) {
    n *= 2;
}

BetterBool betterBoolId(BetterBool x) {
    return x;
}

BetterBool takesBetterBoolCallback(BetterBoolCallback callback, BetterBool value) {
    return callback(value);
}

BetterBool takesBetterBoolsCallback(BetterBoolsCallback callback, BetterBool value) {
    return callback(value);
}

void throwsBaseException() {
    throw BaseException();
}

void throwsFileException() {
    throw FileException();
}

void throwsReadException() {
    throw ReadException();
}

void throwsWriteException() {
    throw WriteException();
}

void throwsPtrCtr() {
    throw PtrCtr();
}

void throwsAny(int i) {
    switch (i) {
    case 0: throw 0;  // A number.
    case 1: throw "hi";  // A C string.
    case 2: throw nullptr;  // A pointer.
    case 3: throw std::string("hi");  // A class non-exception class to Hoppy.
    }
}

int invokeThrowingCallback(ThrowingCallback cb) {
    try {
        cb();
    } catch (const WriteException&) {
        return 4;
    } catch (const ReadException&) {
        return 3;
    } catch (const FileException&) {
        return 2;
    } catch (const BaseException&) {
        return 1;
    } catch (...) {
        return 0;
    }
    return -1;
}

bool throwingReturnBool() {
    throw BaseException();
}

int throwingReturnInt() {
    throw BaseException();
}

IntBox throwingReturnIntBox() {
    throw BaseException();
}

int throwingMakeBoxByValueCallbackDriver(ThrowingMakeBoxByValueCallback cb, int value) {
    return cb(value).get();
}
