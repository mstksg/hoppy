#ifndef CPPOP_INHERITANCE_HPP
#define CPPOP_INHERITANCE_HPP

class InheritanceA {
public:
    virtual int aFoo() const { return 1; }
    virtual int aBar() const { return 2; }
};

class InheritanceB {
public:
    virtual int bFoo() const = 0;
};

class InheritanceC : public InheritanceA, public InheritanceB {
public:
    virtual int aBar() const { return 20; }
    virtual int bFoo() const { return 30; }
};

#endif
