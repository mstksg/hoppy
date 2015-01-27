#include "buffers.h"
#include <cstdlib>
#include <cstring>
#include <iostream>

namespace cppop {

BufferReader::BufferReader(const char* buffer, size_t size) :
    start_(buffer),
    end_(buffer + size),
    cur_(buffer),
    size_(size) {}

size_t BufferReader::remainingBytes() const {
    return end_ - cur_;
}

const char* BufferReader::read(size_t bytes) {
    if (cur_ + bytes > end_) {
        abort();
    }
    const char* p = cur_;
    cur_ += bytes;
    return p;
}

SizedBuffer::SizedBuffer() :
    start_((char*) malloc(DEFAULT_SIZE)),
    size_(DEFAULT_SIZE) {
    if (start_ == NULL) {
        std::cerr << "SizedBuffer::SizedBuffer(): Failed to malloc "
                  << DEFAULT_SIZE << "byte(s), aborting.\n";
        abort();
    }
}

SizedBuffer::SizedBuffer(size_t initialSize) :
    start_((char*) malloc(initialSize)),
    size_(initialSize) {
    if (start_ == NULL) {
        std::cerr << "SizedBuffer::SizedBuffer(size_t): Failed to malloc "
                  << initialSize << "byte(s), aborting.\n";
        abort();
    }
}

SizedBuffer::~SizedBuffer() {
    free(start_);
}

char* SizedBuffer::buffer() const {
    return start_;
}

size_t SizedBuffer::size() const {
    return size_;
}

void SizedBuffer::ensureSize(size_t bytes) {
    if (size_ < bytes) {
        start_ = (char*) realloc(start_, bytes);
        if (start_ == NULL) {
            std::cerr << "SizedBuffer::ensureSize: Failed to resize from "
                      << size_ << " to " << bytes << ", aborting.\n";
            abort();
        }
        size_ = bytes;
    }
}

BufferReader* SizedBuffer::reader() const {
    return new BufferReader(start_, size_);
}

BufferReader* SizedBuffer::reader(size_t maxReaderBytes) const {
    return new BufferReader(start_, size_ > maxReaderBytes ? maxReaderBytes : size_);
}

WritableBuffer::WritableBuffer() :
    writtenCount_(0) {}

size_t WritableBuffer::writtenSize() const {
    return writtenCount_;
}

char* WritableBuffer::alloc(size_t bytes) {
    size_t neededSize = writtenCount_ + bytes;
    if (neededSize > size()) {
        size_t naturalGrowth = (size_t) (1.3 * size());
        size_t newSize =
            naturalGrowth > neededSize ? naturalGrowth : neededSize;
        ensureSize(newSize);
    }
    char* p = buffer() + writtenCount_;
    writtenCount_ += bytes;
    return p;
}

void WritableBuffer::resetAlloc() {
    writtenCount_ = 0;
}

std::string decodeStdString(BufferReader* reader) {
    size_t size = *(size_t*)reader->read(sizeof(size_t));
    std::cerr << "Reading string of size 0x" << std::hex << (size & 0xff) << ".\n";
    const char* p = reader->read(size);
    return std::string(p, size);
}

void encodeStdString(const std::string& str, WritableBuffer* buf) {
    size_t size = str.size();
    char* p = buf->alloc(sizeof(size_t) + size);
    *(size_t*)p = size;
    p += sizeof(size_t);
    memcpy(p, str.c_str(), size);
}

}
