#ifndef CPPOP_INTBOX_HPP
#define CPPOP_INTBOX_HPP

class IntBox {
public:
    IntBox() : n_(0) {}
    IntBox(int n) : n_(n) {}
    int get() const { return n_; }
    void set(int n) { n_ = n; }

private:
    int n_;
};

#endif
