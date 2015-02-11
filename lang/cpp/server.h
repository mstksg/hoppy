#ifndef CPPOP_SERVER_H
#define CPPOP_SERVER_H

#include <boost/noncopyable.hpp>
#include <boost/thread/thread.hpp>
#include <list>
#include <map>
#include <string>

#include "common.h"
#include "idspace.h"
#include "interface.h"
#include "message.h"
#include "mvar.h"

namespace cppop {

class Listener;

class Server : private boost::noncopyable {
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

    void execute(const CalcMessage& message);
    void execute(const CallMessage& message);
    void execute(const RetnMessage& message);

    FILE* getLogFile();
    boost::mutex* getLogMutex();

    friend class Callback;
    friend class ThreadRequestRegistrationGuard;

private:
    void send(const Message& message);

    const Export* lookup(const std::string& name);

    void registerThreadRequest(request_id_t requestId);

    void unregisterThreadRequest(request_id_t requestId);

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
    scoped_ptr<Listener> listener_;
    boost::mutex readMutex_;
    boost::mutex writeMutex_;
    boost::mutex logMutex_;

    // Server requests.
    boost::mutex serverRequestMutex_;
    IdSpace<request_id_t> serverRequestIds_;
    //std::map<request_id_t, ServerRequest*> serverRequests_;

    boost::mutex threadRequestsMutex_;
    std::map<boost::thread::id, std::list<request_id_t> > threadRequests_;
};

class ThreadRequestRegistrationGuard : private boost::noncopyable {
public:
    ThreadRequestRegistrationGuard(Server* server, request_id_t requestId);
    ~ThreadRequestRegistrationGuard();

private:
    Server* const server_;
    const request_id_t requestId_;
};

class Listener : private boost::noncopyable {
public:
    Listener(Server* server, FILE* inputFile);

    void operator()();

    MVar<MVar<const Message*>*>* getListenerVar() { return &listenerVar_; }

private:
    Listener(const Listener&);

    Server* const server_;
    FILE* const inputFile_;
    MVar<MVar<const Message*>*> listenerVar_;
};

class Runner : private boost::noncopyable {
public:
    Runner(Server* server, MVar<MVar<const Message*>*>* listenerVar);

    void operator()();

private:
    Server* const server_;
    MVar<MVar<const Message*>*>* const listenerVar_;
    MVar<const Message*> messageVar_;
};

} // namespace cppop

#endif // CPPOP_SERVER_H
