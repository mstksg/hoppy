#ifndef DRIVER_H
#define DRIVER_H

#include "interface.h"

namespace cppop {

int executeWithArgs(
    int argc,
    const char* const argv[],
    size_t interfaceCount,
    InterfaceFn interfaces[]);

}

#endif // DRIVER_H
