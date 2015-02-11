#include "common.h"

namespace cppop {

void hexDump(FILE* const file, const char* const buffer, const size_t size) {
    const size_t maxBytesOnLine = 16;
    size_t currentOffset = 0;

    while (currentOffset < size) {
        size_t bytesOnLine = size - currentOffset;
        if (bytesOnLine > maxBytesOnLine) {
            bytesOnLine = maxBytesOnLine;
        }

        fprintf(file, "%08lx ", currentOffset);
        for (size_t i = 0; i < bytesOnLine; ++i) {
            fprintf(file, " %02hhx", buffer[currentOffset + i]);
        }
        for (size_t i = 0, end = (maxBytesOnLine - bytesOnLine) * 3 + 2; i < end; ++i) {
            fputc(' ', file);
        }
        for (size_t i = 0; i < bytesOnLine; ++i) {
            const char c = buffer[currentOffset + i];
            fputc(c >= 0x20 && c <= 0x7e ? c : '.', file);
        }
        fputc('\n', file);

        currentOffset += bytesOnLine;
    }
}

}
