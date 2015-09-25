#ifndef CPPOP_FUNCTIONS_HPP
#define CPPOP_FUNCTIONS_HPP

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

#endif
