#include "flob.hpp"

void takesFlubValues(FlubClass*, FlubEnum, int) {}

FlubClass* returnsFlubClass() {
    return 0;
}

FlubEnum returnsFlubEnum() {
    return OPTION_B;
}

int returnsFlubBitspace() {
    return OPTION_A | OPTION_C;
}
