#ifndef HOPPY_FLOB_HPP
#define HOPPY_FLOB_HPP

#include "flub.hpp"
#include "flubm.hpp"

class FlobClass {
public:
    void invokeCallback(FlubCallback) const {}
};

void takesFlubValues(FlubClass*, FlubEnum, int);

FlubClass* returnsFlubClass();

FlubEnum returnsFlubEnum();

int returnsFlubBitspace();

#endif
