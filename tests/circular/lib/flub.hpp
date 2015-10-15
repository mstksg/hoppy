#ifndef HOPPY_FLUB_HPP
#define HOPPY_FLUB_HPP

extern char flubVar;
extern const char flubVarConst;

enum FlubEnum {
    OPTION_A = 0x1,
    OPTION_B = 0x2,
    OPTION_C = 0x4,
};

class FlubClass {};

// Break circularity C++-style.
class FlobClass;

void takesFlobValues(FlobClass*);

#endif
