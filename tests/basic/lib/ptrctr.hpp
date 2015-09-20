#ifndef CPPOP_PTRCTR_HPP
#define CPPOP_PTRCTR_HPP

// Counts the number of constructions and destructions objects of this class
// undergo.
class PtrCtr {
public:
    PtrCtr();
    PtrCtr(const PtrCtr&);
    PtrCtr(PtrCtr&&);
    ~PtrCtr();

    static int resetCounters();
    static int getConstructionCount();
    static int getDestructionCount();

private:
    static int constructions_;
    static int destructions_;

    int myNum = 0;
};

#endif
