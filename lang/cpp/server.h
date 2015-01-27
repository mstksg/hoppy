#ifndef CPPOP_SERVER_H
#define CPPOP_SERVER_H

#include <boost/thread/thread.hpp>
#include <list>
#include <map>
#include <string>

#include "common.h"
#include "interface.h"

namespace cppop {

class Server;
class ServerThreadStarter;

class Server {
public:
    Server();
    virtual ~Server();

    // The server takes ownership of the interface.
    bool addInterface(const Interface* interface);

    // Starts the server running, communicating on the given paths and using
    // the given number of threads to process incoming requests.  Returns true
    // on success and false if some error occurred and the server could not
    // start.
    //
    // Protocol logs will be appended to the file specified by 'logPath', if
    // the path is not empty.
    bool start(
        const std::string inputPath,
        const std::string outputPath,
        const std::string logPath,
        const unsigned int numThreads);

    // Shuts down the server.  Kills server threads and blocks until the server
    // threads have stopped (thus all outstanding requests will be completed),
    // then returns true.  Returns false if the server was not running in the
    // first place.
    bool stop();

    // Blocks until the server is no longer running.  The server may be stopped
    // by a call to stop() in another thread or by a request from a client for
    // the server to stop.  If called when the server is not running, this
    // function returns immediately.
    void wait();

    friend class Callback;
    friend class ServerThreadStarter;

private:
    Server(const Server&);

    void run(unsigned int threadNum);

    const Export* lookup(const std::string& name);

    void hexDump(FILE* file, const char* buffer, size_t size) const;

    // Interface configuration.
    std::map<std::string, const Interface*> interfaces_;
    std::map<std::string, const Export> exports_;

    // Runtime state.
    bool started_;
    std::string inputPath_;
    std::string outputPath_;
    std::string logPath_;
    FILE* inputReadFile_;
    FILE* inputWriteFile_;
    FILE* outputWriteFile_;
    FILE* logFile_;
    boost::thread_group threads_;
    boost::mutex readMutex_;
    boost::mutex writeMutex_;
    boost::mutex logMutex_;
};

} // namespace cppop

#endif // CPPOP_SERVER_H
