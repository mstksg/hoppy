#include "buffer.h"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include "callback.h"

namespace cppop {

SizedBuffer::SizedBuffer() :
    buffer_(static_cast<char*>(malloc(DEFAULT_CAPACITY))),
    capacity_(DEFAULT_CAPACITY),
    size_(0) {
    if (buffer_.get() == NULL) {
        std::cerr << "SizedBuffer::SizedBuffer(): Failed to malloc "
                  << DEFAULT_CAPACITY << "byte(s), aborting.\n";
        abort();
    }
}

SizedBuffer::SizedBuffer(const SizedBuffer& other) :
    buffer_(static_cast<char*>(malloc(other.size_))),
    capacity_(other.size_),
    size_(other.size_) {
    memcpy(buffer_.get(), other.buffer_.get(), size_);
}

SizedBuffer::SizedBuffer(size_t initialSize) :
    buffer_(static_cast<char*>(malloc(initialSize))),
    capacity_(initialSize),
    size_(initialSize) {
    if (buffer_.get() == NULL) {
        std::cerr << "SizedBuffer::SizedBuffer(size_t): Failed to malloc "
                  << initialSize << "byte(s), aborting.\n";
        abort();
    }
}

SizedBuffer& SizedBuffer::operator=(const SizedBuffer& other) {
    ensureSize(other.size_);
    memcpy(buffer_.get(), other.buffer_.get(), size_);
    return *this;
}

char* SizedBuffer::buffer() const {
    return buffer_.get();
}

char* SizedBuffer::at(size_t offset) const {
    return buffer_.get() + offset;
}

size_t SizedBuffer::size() const {
    return size_;
}

void SizedBuffer::ensureSize(size_t bytes) {
    if (capacity_ < bytes) {
        buffer_.assign((char*)realloc(buffer_.release(), bytes));
        if (buffer_.get() == NULL) {
            std::cerr << "SizedBuffer::ensureSize: Failed to resize from "
                      << size_ << " to " << bytes << ", aborting.\n";
            abort();
        }
        capacity_ = bytes;
    }
    size_ = bytes;
}

SizedBufferReader::SizedBufferReader(const SizedBuffer& buffer) :
    buffer_(buffer), offset_(0) {}

const char* SizedBufferReader::cursor() const {
    return buffer_.at(offset_);
}

size_t SizedBufferReader::remainingBytes() const {
    return buffer_.size() - offset_;
}

const char* SizedBufferReader::read(size_t bytes) {
    const size_t start = offset_;
    offset_ += bytes;
    return buffer_.at(start);
}

void SizedBufferReader::readTo(char* const target, size_t size) {
    memcpy(target, read(size), size);
}

SizedBufferWriter::SizedBufferWriter(SizedBuffer& buffer) :
    buffer_(buffer), offset_(0) {}

SizedBuffer& SizedBufferWriter::buffer() {
    return buffer_;
}

size_t SizedBufferWriter::writtenSize() const {
    return offset_;
}

void SizedBufferWriter::reset() {
    offset_ = 0;
}

size_t SizedBufferWriter::alloc(size_t bytes) {
    const size_t start = offset_;
    buffer_.ensureSize(offset_ + bytes);
    offset_ += bytes;
    return start;
}

char* SizedBufferWriter::allocPointer(size_t bytes) {
    return buffer_.at(alloc(bytes));
}

void SizedBufferWriter::write(const char* bytes, size_t size) {
    memcpy(allocPointer(size), bytes, size);
}

void SizedBufferWriter::write(const SizedBuffer& source) {
    write(source.buffer(), source.size());
}

std::string decodeStdString(Server&, SizedBufferReader& reader) {
    size_t size;
    reader.readLiteralTo(&size);
    const char* p = reader.read(size);
    return std::string(p, size);
}

void encodeStdString(const std::string& str, SizedBufferWriter& writer) {
    size_t size = str.size();
    char* p = writer.allocPointer(sizeof(size_t) + size);
    *(size_t*)p = size;
    p += sizeof(size_t);
    memcpy(p, str.c_str(), size);
}

Callback decodeCallback(Server& server, SizedBufferReader& reader) {
    callback_id_t id;
    reader.readLiteralTo(&id);
    return Callback(server, id);
}

void encodeCallback(Callback callback, SizedBufferWriter& writer) {
    writer.writeLiteral(callback.getId());
}

}
