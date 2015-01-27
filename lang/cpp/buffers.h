#ifndef SERIAL_DEFS_H
#define SERIAL_DEFS_H

#include <cstdlib>
#include <stdint.h>
#include <string>

namespace cppop {

//using std::cerr;
//using std::string;

//typedef uint8_t btype_t;

//typedef uint32_t bsize_t;

//enum SerialErr {
//    SER_OK = 0,
//    SER_BADTYPE,
//};

//enum BType {
//    BT_INVALID = 0x0,
//    BT_BOOL = 0x1,
//    BT_INT8 = 0x2,
//    BT_UINT8 = 0x3,
//    BT_INT16 = 0x4,
//    BT_UINT16 = 0x5,
//    BT_INT32 = 0x6,
//    BT_UINT32 = 0x7,
//    BT_INT64 = 0x8,
//    BT_UINT64 = 0x9,
//    BT_FLOAT = 0xa,
//    BT_DOUBLE = 0xb,

//    BT_MAP = 0x10,
//    BT_ARRAY = 0x11,
//    BT_STRING = 0x12,
//};

//class BTyped {
//public:
//    BTyped(const void* ptr) : ptr((char*)ptr) {}

//    bsize_t bsize() const { return sizeof(btype_t) + bsizeInternal(); }
//    BType type() const { return (BType)*(bsize_t*)ptr; }

//    bool asBool() const;
//    char asChar() const;
//    unsigned char asUChar() const;
//    short asShort() const;
//    unsigned short asUShort() const;
//    int asInt() const;
//    unsigned int asUInt() const;
//    long asLong() const;
//    unsigned long asULong() const;
//    long long asLLong() const;
//    unsigned long long asULLong() const;
//    float asFloat() const;
//    double asDouble() const;
//    string asString() const;

//    const void* as(BType expectedType) const;
//    const void* asSigned(size_t typeSize) const;
//    const void* asUnsigned(size_t typeSize) const;

//private:
//    char* const ptr;

//    bsize_t bsizeInternal() const;
//};

//class BArray {
//public:
//    BArray(const void* ptr) :
//        count_(*(bsize_t*)ptr),
//        first((const char*)ptr + sizeof(bsize_t)) {}

//    bsize_t bsize() const;
//    int count() const { return count_; }
//    BTyped get(bsize_t index) const;

//private:
//    const bsize_t count_;
//    const char* first;
//};

//static BArray asArray(const BTyped& x) {
//    return BArray(x.as(BT_ARRAY));
//}

//class BMap {
//public:
//    BMap(const void* ptr) :
//        count_(*(bsize_t*)ptr),
//        first((const char*)ptr + sizeof(bsize_t)) {}

//    bsize_t bsize() const;
//    int count() const { return count_; }
//    // How to represent Maybe?
//    //BTyped get(const string& key) const;
//    //BTyped get(const char* key) const;

//private:
//    const bsize_t count_;
//    const char* const first;
//};

//static BMap asMap(const BTyped& x) {
//    return BMap(x.as(BT_MAP));
//}

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

//template <typename T>
//void serialize(T obj, WritableBuffer* buf);

}

#endif // SERIAL_DEFS_H
