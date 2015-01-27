#ifndef SERIAL_DEFS_H
#define SERIAL_DEFS_H

#include <cstdlib>
#include <stdint.h>
#include <string>

namespace cppop {

class BufferReader {
public:
    BufferReader(const char* buffer, size_t size);

    size_t remainingBytes() const;

    const char* read(size_t bytes);

private:
    const char* const start_;
    const char* const end_;
    const char* cur_;
    const size_t size_;
};

class SizedBuffer {
public:
    SizedBuffer();
    explicit SizedBuffer(size_t initialSize);
    virtual ~SizedBuffer();
    char* buffer() const;
    size_t size() const;
    void ensureSize(size_t bytes);

    // The reader is only valid while the buffer is not resized.
    BufferReader* reader() const;
    BufferReader* reader(size_t maxReaderBytes) const;

private:
    SizedBuffer(const SizedBuffer&);

    static const size_t DEFAULT_SIZE = 256;

    char* start_;
    size_t size_;
};

class WritableBuffer : public SizedBuffer {
public:
    WritableBuffer();

    size_t writtenSize() const;

    char* alloc(size_t bytes);

    void resetAlloc();

    //char* allocTyped(BType type, bsize_t bytes);

private:
    size_t writtenCount_;
};

std::string decodeStdString(BufferReader* reader);

void encodeStdString(const std::string& str, WritableBuffer* buf);

}

#endif // SERIAL_DEFS_H
