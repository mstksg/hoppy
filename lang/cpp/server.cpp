#include "server.h"

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <sys/stat.h>
#include <unistd.h>

#include "buffers.h"

namespace cppop {

class ServerThreadStarter {
public:
    ServerThreadStarter(Server* server, unsigned int threadNum) :
        server_(server),
        threadNum_(threadNum) {}

    void operator()() {
        server_->run(threadNum_);
    }

private:
    Server* const server_;
    const int threadNum_;
};

Server::Server() :
    started_(false),
    inputReadFile_(NULL),
    inputWriteFile_(NULL),
    outputWriteFile_(NULL),
    logFile_(NULL) {}

Server::~Server() {
    stop();

    for (std::map<std::string, const Interface*>::iterator it = interfaces_.begin();
         it != interfaces_.end(); ++it) {
        delete it->second;
    }
}

bool Server::addInterface(const Interface *interface) {
    const std::map<std::string, const Interface*>::const_iterator existingInterface =
        interfaces_.find(interface->name());
    if (existingInterface != interfaces_.end()) {
        std::cerr << "Server::addInterface: Interface named \""
                  << interface->name() << " declared twice, failing.\n";
        return false;
    }
    interfaces_[interface->name()] = interface;

    const ExportMap& exports = interface->exportMap();
    for (ExportMap::const_iterator it = exports.begin(); it != exports.end(); ++it) {
        const Export& exp = it->second;
        std::map<std::string, const Export>::iterator existingExport =
            exports_.find(exp.name());
        if (existingExport != exports_.end()) {
            std::cerr << "Server::addInterface: Duplicate export \""
                      << exp.name() << "\" from interfaces \""
                      << existingExport->second.interface()->name()
                      << "\" and \"" << interface->name() << "\".\n";
            return false;
        }
        exports_.insert(std::make_pair(exp.name(), exp));
    }

    return true;
}

bool Server::start(
    const std::string inputPath,
    const std::string outputPath,
    const std::string logPath,
    const unsigned int numThreads) {
    if (numThreads < 1) {
        std::cerr << "Server::start: Number of threads (" << numThreads
                  << ") must be positive.\n";
        return false;
    }

    if (started_) {
        return false;
    }

    inputPath_ = inputPath;
    outputPath_ = outputPath;
    logPath_ = logPath;

    std::cerr << "Server::start: Opening input for read.\n";  //zz
    inputReadFile_ = fopen(inputPath.c_str(), "r");
    if (inputReadFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << inputPath_ << "\" for reading.\n";
        goto failOpenInputRead;
    }
    std::cerr << "Server::start: Opening input for write.\n";  //zz
    inputWriteFile_ = fopen(inputPath.c_str(), "w");
    if (inputWriteFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << inputPath_ << "\" for writing.\n";
        goto failOpenInputWrite;
    }
    std::cerr << "Server::start: Opening output for write.\n";  //zz
    outputWriteFile_ = fopen(outputPath.c_str(), "w");
    if (outputWriteFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << outputPath_ << "\" for writing.\n";
        goto failOpenOutputWrite;
    }

    if (!logPath.empty()) {
        std::cerr << "Server::start: Opening log file for append.\n";  //zz
        logFile_ = fopen(logPath.c_str(), "a");
        if (logFile_ == NULL) {
            std::cerr << "Server::start: Failed to open \"" << logPath
                      << "\" for appending.\n";
            goto failOpenLogAppend;
        }
    }

    std::cerr << "Server::start: Everything opened.\n";  //zz

    for (unsigned int i = 0; i < numThreads; ++i) {
        threads_.create_thread(ServerThreadStarter(this, i));
    }

    started_ = true;
    return true;

failOpenLogAppend:
    fclose(outputWriteFile_);
    outputWriteFile_ = NULL;
failOpenOutputWrite:
    fclose(inputWriteFile_);
    inputWriteFile_ = NULL;
failOpenInputWrite:
    fclose(inputReadFile_);
    inputReadFile_ = NULL;
failOpenInputRead:
    return false;
}

bool Server::stop() {
    // TODO Stop the server.
    return false;
}

void Server::wait() {
    threads_.join_all();
}

void Server::run(const unsigned int threadNum) {
    request_id_t requestId;
    size_t recvSize;
    SizedBuffer recvBuffer;
    WritableBuffer sendBuffer;
    size_t n;

    while (true) {
        boost::this_thread::interruption_point();

        {
            boost::lock_guard<boost::mutex> readLock(readMutex_);

            // First read the header (sequence number and request body size).
            recvSize = sizeof(requestId) + sizeof(recvSize);
            recvBuffer.ensureSize(recvSize);  // TODO Lift this out of the loop (the buffer doesn't shrink).
            n = fread(recvBuffer.buffer(), recvSize, 1, inputReadFile_);
            if (n < 1) {
                if (feof(inputReadFile_)) {
                    std::cerr << "Server::run: EOF received, exiting.\n";
                } else {
                    std::cerr << "Server::run: Error reading request size; exiting.\n";
                }
                break;
            }

            // Decode the header.
            {
                BufferReader* const reader = recvBuffer.reader(recvSize);
                requestId = *(request_id_t*)reader->read(sizeof(requestId));
                recvSize = *(size_t*)reader->read(sizeof(recvSize));
                if (logFile_ != NULL) {
                    boost::lock_guard<boost::mutex> logLock(logMutex_);
                    fprintf(logFile_,
                        "\n- Thread#%u: Header received, seq num " PRI_REQUEST_ID ", body size %zu.\n",
                        threadNum, requestId, recvSize);
                    fflush(logFile_);
                }
                if (reader->remainingBytes() != 0) {
                    std::cerr << "Server::run: Internal error parsing client request header.\n";
                    std::cerr << "(Expected " << (sizeof(requestId) + sizeof(recvSize))
                              << " bytes, still have " << reader->remainingBytes() << " left.)\n";
                    break;
                }
                delete reader;
            }

            recvBuffer.ensureSize(recvSize);
            n = fread(recvBuffer.buffer(), 1, recvSize, inputReadFile_);
            if (n < recvSize) {
                if (feof(inputReadFile_)) {
                    std::cerr << "Server::run: Received EOF after " << n
                              << " message byte(s), exiting.\n";
                } else {
                    std::cerr << "Server::run: Received error after " << n
                              << " message byte(s), exiting.\n";
                }
                break;
            }
            if (logFile_ != NULL) {
                boost::lock_guard<boost::mutex> logLock(logMutex_);
                fprintf(logFile_,
                    "\n- Thread#%u: Request received, seq num " PRI_REQUEST_ID ", body size %zu:\n",
                    threadNum,
                    requestId,
                    recvSize);
                hexDump(logFile_, recvBuffer.buffer(), recvSize);
                fflush(logFile_);
            }
        }

        // Look up the export.
        const std::string name(recvBuffer.buffer());
        if (name.empty()) {  // An empty name can be used to wake the thread up.
            continue;
        }
        const Export* const exp = lookup(name);
        if (exp == NULL) {
            std::cerr << "Server::run: Unknown export \"" << name
                      << "\" called, ignoring.\n";
            continue;
        }
        std::cerr << "#" << name << "\n";

        // Call the exported function.
        sendBuffer.resetAlloc();
        *(request_id_t*)sendBuffer.alloc(sizeof(request_id_t)) = requestId;
        size_t* const responseSize = (size_t*)sendBuffer.alloc(sizeof(size_t));
        {
            BufferReader* const reader = recvBuffer.reader(recvSize);
            reader->read(name.length() + 1);
            exp->exportFn()(reader, &sendBuffer);
            if (reader->remainingBytes() != 0) {
                std::cerr << "Server::run: " << reader->remainingBytes()
                          << " argument byte(s) left unread in call to \""
                          << name << "\".\n";
            }
            delete reader;
        }

        {
            boost::lock_guard<boost::mutex> writeLock(writeMutex_);

            *responseSize = sendBuffer.writtenSize() - sizeof(request_id_t) - sizeof(size_t);
            if (logFile_ != NULL) {
                fprintf(logFile_,
                    "\n- Thread#%u: Sending response, seq num " PRI_REQUEST_ID ", total size %zu:\n",
                    threadNum,
                    requestId,
                    sendBuffer.writtenSize());
                hexDump(logFile_, sendBuffer.buffer(), sendBuffer.writtenSize());
                fflush(logFile_);
            }
            n = fwrite(sendBuffer.buffer(), 1, sendBuffer.writtenSize(), outputWriteFile_);
            if (n < sendBuffer.writtenSize()) {
                std::cerr << "Server::run: Error writing response, exiting.\n";
                break;
            }
            fflush(outputWriteFile_);  // TODO Check for error.
        }
    }
}

const Export* Server::lookup(const std::string& name) {
    std::map<std::string, const Export>::iterator it = exports_.find(name);
    return it == exports_.end() ? NULL : &it->second;
}

void Server::hexDump(FILE* file, const char* const buffer, const size_t size) const {
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

} // namespace cppop
