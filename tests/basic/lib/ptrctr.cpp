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
