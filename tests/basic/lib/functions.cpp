#include "functions.hpp"

int piapprox() {
    return 4;
}

long timesTwo(long n) {
    return n * 2;
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
