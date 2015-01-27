#include "buffers.h"
#include <cstdlib>
#include <cstring>
#include <iostream>

namespace cppop {

//static const BType signedSizesToTypes[] = {
//    BT_INVALID,
//    BT_INT8,
//    BT_INT16,
//    BT_INVALID,
//    BT_INT32,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INT64,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//};

//static const BType unsignedSizesToTypes[] = {
//    BT_INVALID,
//    BT_UINT8,
//    BT_UINT16,
//    BT_INVALID,
//    BT_UINT32,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_UINT64,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//    BT_INVALID,
//};

//bsize_t BTyped::bsizeInternal() const {
//    switch (type()) {
//    case BT_BOOL:
//    case BT_INT8:
//    case BT_UINT8:
//        return 1;

//    case BT_INT16:
//    case BT_UINT16:
//        return 2;

//    case BT_INT32:
//    case BT_UINT32:
//    case BT_FLOAT:
//        return 4;

//    case BT_INT64:
//    case BT_UINT64:
//    case BT_DOUBLE:
//        return 8;

//    case BT_MAP:
//        return asMap(*this).bsize();

//    case BT_ARRAY:
//        return asArray(*this).bsize();

//    case BT_STRING:
//        return sizeof(bsize_t) + *(bsize_t*)as(BT_STRING);

//    case BT_INVALID:
//    default:
//        std::cerr << "BTyped::bsizeInternal: Invalid type " << type() << ".\n" << std::flush;
//        abort();
//    }
//}

//bool BTyped::asBool() const {
//    return *(bool*)as(BT_BOOL);
//}

//char BTyped::asChar() const {
//    return *(char*)asSigned(sizeof(char));
//}

//unsigned char BTyped::asUChar() const {
//    return *(unsigned char*)asUnsigned(sizeof(unsigned char));
//}

//short BTyped::asShort() const {
//    return *(short*)asSigned(sizeof(short));
//}

//unsigned short BTyped::asUShort() const {
//    return *(unsigned short*)asUnsigned(sizeof(unsigned short));
//}

//int BTyped::asInt() const {
//    return *(int*)asSigned(sizeof(int));
//}

//unsigned int BTyped::asUInt() const {
//    return *(unsigned int*)asUnsigned(sizeof(unsigned int));
//}

//long BTyped::asLong() const {
//    return *(long*)asSigned(sizeof(long));
//}

//unsigned long BTyped::asULong() const {
//    return *(unsigned long*)asUnsigned(sizeof(unsigned long));
//}

//long long BTyped::asLLong() const {
//    return *(long long*)asSigned(sizeof(long long));
//}

//unsigned long long BTyped::asULLong() const {
//    return *(unsigned long long*)asUnsigned(sizeof(unsigned long long));
//}

//float BTyped::asFloat() const {
//    return *(float*)as(BT_FLOAT);
//}

//double BTyped::asDouble() const {
//    return *(double*)as(BT_DOUBLE);
//}

//string BTyped::asString() const {
//    char *str = (char *) as(BT_STRING);
//    size_t len = (size_t) * (bsize_t *) str;
//    return string(str + sizeof(bsize_t), len);
//}

//const void* BTyped::as(BType expectedType) const {
//    if (type() != expectedType) {
//        fail("BTyped::as: Wrong type.");
//    }
//    return ptr + sizeof(btype_t);
//}

//const void* BTyped::asSigned(size_t typeSize) const {
//    if (typeSize < 1 || typeSize >= sizeof(signedSizesToTypes) / sizeof(signedSizesToTypes[0])) {
//        fail("BTyped::asSigned:: Bad sizeof.");
//    }
//    return as(signedSizesToTypes[typeSize]);
//}

//const void* BTyped::asUnsigned(size_t typeSize) const {
//    if (typeSize < 1 || typeSize >= sizeof(unsignedSizesToTypes) / sizeof(unsignedSizesToTypes[0])) {
//        fail("BTyped::asUnsigned:: Bad sizeof.");
//    }
//    return as(unsignedSizesToTypes[typeSize]);
//}

//bsize_t BArray::bsize() const {
//    bsize_t total = sizeof(bsize_t), size;
//    const char* p = first;
//    for (bsize_t i = 0; i < count_; ++i) {
//        size = BTyped(p).bsize();
//        total += size;
//        p += size;
//    }
//    return total;
//}

//BTyped BArray::get(bsize_t index) const {
//    if (index >= count_) {
//        std::cerr << "BArray::get: index " << index << " >= count " << count_ << ".";
//        abort();
//    }

//    const char* p = first;
//    for (bsize_t i = index; i > 0; --i) {
//        p += BTyped(p).bsize();
//    }
//    return BTyped(p);
//}

//bsize_t BMap::bsize() const {
//    bsize_t total = sizeof(bsize_t), bsize;
//    size_t size;
//    const char* p = first;
//    for (bsize_t i = 0; i < count_; ++i) {
//        // Skip over the key.
//        size = strlen(p);
//        total += size;
//        p += size;
//        // Skip over the value.
//        bsize = BTyped(p).bsize();
//        total += bsize;
//        p += bsize;
//    }
//    return total;
//}

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

//char* WritableBuffer::allocTyped(BType type, bsize_t bytes) {
//    char* p = allocBytes(sizeof(btype_t) + bytes);
//    *(btype_t*)p = type;
//    return p + sizeof(btype_t);
//}

//template <>
//void serialize(bool x, WritableBuffer* buf) {
//    *(bool*)buf->allocBytes(sizeof(bool)) = x;
//}

//template <>
//void serialize(char x, WritableBuffer* buf) {
//    *(char*)buf->allocBytes(sizeof(char)) = x;
//}

//template <>
//void serialize(unsigned char x, WritableBuffer* buf) {
//    *(unsigned char*)buf->allocBytes(sizeof(unsigned char)) = x;
//}

//template <>
//void serialize(short x, WritableBuffer* buf) {
//    *(short*)buf->allocBytes(sizeof(short)) = x;
//}

//template <>
//void serialize(unsigned short x, WritableBuffer* buf) {
//    *(unsigned short*)buf->allocBytes(sizeof(unsigned short)) = x;
//}

//template <>
//void serialize(int x, WritableBuffer* buf) {
//    *(int*)buf->allocBytes(sizeof(int)) = x;
//}

//template <>
//void serialize(unsigned int x, WritableBuffer* buf) {
//    *(unsigned int*)buf->allocBytes(sizeof(unsigned int)) = x;
//}

//template <>
//void serialize(long x, WritableBuffer* buf) {
//    *(long*)buf->allocBytes(sizeof(long)) = x;
//}

//template <>
//void serialize(unsigned long x, WritableBuffer* buf) {
//    *(unsigned long*)buf->allocBytes(sizeof(unsigned long)) = x;
//}

//template <>
//void serialize(long long x, WritableBuffer* buf) {
//    *(long long*)buf->allocBytes(sizeof(long long)) = x;
//}

//template <>
//void serialize(unsigned long long x, WritableBuffer* buf) {
//    *(unsigned long long*)buf->allocBytes(sizeof(unsigned long long)) = x;
//}

//template <>
//void serialize(float x, WritableBuffer* buf) {
//    *(float*)buf->allocBytes(sizeof(float)) = x;
//}

//template <>
//void serialize(double x, WritableBuffer* buf) {
//    *(double*)buf->allocBytes(sizeof(double)) = x;
//}

//template <typename T>
//void serialize(T* x, WritableBuffer* buf) {
//    *(void*)buf->allocBytes(sizeof(void*)) = (void*)x;
//}

}
