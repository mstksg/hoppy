#ifndef COMMON_H
#define COMMON_H

#include <boost/noncopyable.hpp>
#include <cstdio>

namespace cppop {

// TODO Change request_id_t to int32.
typedef int request_id_t;

#define PRI_REQUEST_ID "%d"

// TODO Change callback_id_t to int32.
typedef int callback_id_t;

#define PRI_CALLBACK_ID "%d"

void hexDump(FILE* file, const char* buffer, const size_t size);

template <typename T>
class scoped_ptr : private boost::noncopyable {
public:
    scoped_ptr() : object_(NULL) {}
    explicit scoped_ptr(T* object) : object_(object) {}
    ~scoped_ptr() { delete object_; }

    T& operator*() const { return *object_; }
    T* operator->() const { return object_; }

    // Useful for getting pointers to arrays.
    T* get() const { return object_; }

    void free() {
        delete object_;
        object_ = NULL;
    }

    T* release() {
        T* const object = object_;
        object_ = NULL;
        return object;
    }

    void assign(T* newObject) {
        free();
        object_ = newObject;
    }

private:
    T* object_;
};

}

#endif // COMMON_H
