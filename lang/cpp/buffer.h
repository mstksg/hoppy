#ifndef SERIAL_DEFS_H
#define SERIAL_DEFS_H

#include <boost/noncopyable.hpp>
#include <cstdlib>
#include <stdint.h>
#include <string>
#include "common.h"

namespace cppop {

class Callback;
class Server;
class SizedBufferWriter;

class SizedBuffer {
public:
    SizedBuffer();
    SizedBuffer(const SizedBuffer& other);
    explicit SizedBuffer(size_t initialSize);

    SizedBuffer& operator=(const SizedBuffer& other);

    char* buffer() const;
    char* at(size_t offset) const;
    size_t size() const;
    void ensureSize(size_t bytes);

private:
    static const size_t DEFAULT_CAPACITY = 256;

    scoped_ptr<char> buffer_;
    size_t capacity_;
    size_t size_;
};

class SizedBufferReader : private boost::noncopyable {
public:
    SizedBufferReader(const SizedBuffer& buffer);

    const char* cursor() const;

    size_t remainingBytes() const;

    // The pointer is valid as long as the underlying buffer is not resized.
    const char* read(size_t bytes);

    void readTo(char* target, size_t size);

    template <typename T>
    T readLiteral() {
        return *reinterpret_cast<const T*>(read(sizeof(T)));
    }

    template <typename T>
    void readLiteralTo(T* target) {
        *target = *reinterpret_cast<const T*>(read(sizeof(T)));
    }

private:
    const SizedBuffer& buffer_;
    size_t offset_;
};

class SizedBufferWriter : private boost::noncopyable {
public:
    SizedBufferWriter(SizedBuffer& buffer);

    SizedBuffer& buffer();

    size_t writtenSize() const;

    void reset();

    size_t alloc(size_t bytes);

    // The pointer returned from this function is only valid until the next alloc
    // call is made.
    char* allocPointer(size_t bytes);

    void write(const char* bytes, size_t size);

    void write(const SizedBuffer& source);

    template <typename T>
    void writeLiteral(const T& object) {
        size_t offset = alloc(sizeof(object));
        *(T*)buffer_.at(offset) = object;
    }

private:
    SizedBuffer& buffer_;
    size_t offset_;
};

std::string decodeStdString(Server&, SizedBufferReader& reader);

void encodeStdString(const std::string& str, SizedBufferWriter& writer);

Callback decodeCallback(Server& server, SizedBufferReader& reader);

void encodeCallback(Callback callback, SizedBufferWriter& writer);

}

#endif // SERIAL_DEFS_H
