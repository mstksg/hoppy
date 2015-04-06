#include "server.h"

#include <boost/noncopyable.hpp>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <sys/stat.h>
#include <unistd.h>
#include "buffer.h"
#include "message.h"

namespace {

template <typename T>
class WithId : private boost::noncopyable {
public:
    WithId(IdSpace<T>& space) :
        space_(space),
        id_(space_.request()) {}

    ~WithId() {
        space_.release(id_);
    }

    T id() const { return id_; }

private:
    IdSpace<T>& space_;
    const T id_;
};

}

namespace cppop {

MVar<const Message*>& ServerRequest::getResponseVar() {
    return responseVar_;
}

Server::Server() :
    started_(false),
    inputReadFile_(NULL),
    inputWriteFile_(NULL),
    outputWriteFile_(NULL),
    logFile_(NULL),
    serverRequestIds_("server request ids", -1, -1) {}

Server::~Server() {
    stop();

    for (std::map<std::string, const Interface*>::iterator it = interfaces_.begin();
         it != interfaces_.end(); ++it) {
        delete it->second;
    }
}

bool Server::addInterface(const Interface* interface) {
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
                      << existingExport->second.interface().name()
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

    inputReadFile_ = fopen(inputPath.c_str(), "r");
    if (inputReadFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << inputPath_ << "\" for reading.\n";
        goto failOpenInputRead;
    }
    inputWriteFile_ = fopen(inputPath.c_str(), "w");
    if (inputWriteFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << inputPath_ << "\" for writing.\n";
        goto failOpenInputWrite;
    }
    outputWriteFile_ = fopen(outputPath.c_str(), "w");
    if (outputWriteFile_ == NULL) {
        std::cerr << "Server::start: Failed to open \"" << outputPath_ << "\" for writing.\n";
        goto failOpenOutputWrite;
    }

    if (!logPath.empty()) {
        logFile_ = fopen(logPath.c_str(), "a");
        if (logFile_ == NULL) {
            std::cerr << "Server::start: Failed to open \"" << logPath
                      << "\" for appending.\n";
            goto failOpenLogAppend;
        }
    }

    std::cerr << "Server::start: Cppop running.\n";

    listener_.assign(new Listener(*this, inputReadFile_));
    threads_.create_thread(boost::ref(*listener_));
    {
        MVar<MVar<const Message*>*>& listenerVar = listener_->getListenerVar();
        for (unsigned int i = 0; i < numThreads; ++i) {
            threads_.create_thread(boost::ref(*new Runner(*this, listenerVar)));
        }
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

void Server::invokeCallback(
    callback_id_t callbackId, SizedBuffer* args, SizedBuffer& outResult) {
    WithId<request_id_t> withId(serverRequestIds_);
    const request_id_t requestId = withId.id();
    ThreadRequestRegistrationGuard threadRegistration(*this, requestId);
    ServerRequest request;
    ServerRequestRegistrationGuard requestRegistration(*this, requestId, request);

    CalcMessage message(args);
    message.setCallbackId(callbackId);
    message.setRequestId(requestId);
    message.setParentRequestId(threadRegistration.getParentRequestId());
    send(message);
    while (true) {
        const Message* const response = request.getResponseVar().take();
        const RetnMessage* const retnResponse = dynamic_cast<const RetnMessage*>(response);
        if (retnResponse != NULL) {
            outResult = retnResponse->getBody();
            return;
        } else {
            response->executeOn(*this);
        }
    }
}

void Server::execute(const CalcMessage&) {
    std::cerr << "Server::execute: Can't handle CALC, aborting.\n";
    abort();
}

void Server::execute(const CallMessage& message) {
    ThreadRequestRegistrationGuard registration(*this, message.getRequestId());

    // Look up the requested function.
    boost::optional<const Export&> exp = lookup(message.getFunctionName());
    if (!exp) {
        std::cerr << "Server::run: Unknown export \"" << message.getFunctionName()
                  << "\" called, ignoring.\n";
        return;
    }
    std::cerr << "#" << exp->name() << "\n";

    // Prepare a response message and call the exported function.
    RetnMessage reply(new SizedBuffer());
    reply.setRequestId(message.getRequestId());
    {
        SizedBufferWriter writer(reply.getBody());
        // Add const; the export should not need to modify the argument buffer.
        exp->exportFn()(*this, const_cast<const SizedBuffer&>(message.getBody()), writer);
    }
    send(reply);
}

void Server::execute(const RetnMessage&) {
    std::cerr << "Server::execute: Can't handle a top-level RETN message; aborting.\n";
    abort();
}

boost::optional<ServerRequest*> Server::findServerRequest(const request_id_t requestId) {
    boost::lock_guard<boost::mutex> lock(serverRequestsMutex_);
    std::map<request_id_t, ServerRequest*>::iterator it = serverRequests_.find(requestId);
    if (it == serverRequests_.end()) {
        return boost::none;
    } else {
        return it->second;
    }
}

FILE* Server::getLogFile() {
    return logFile_;
}

boost::mutex& Server::getLogMutex() {
    return logMutex_;
}

void Server::send(const Message& message) {
    SizedBuffer buffer;
    {
        SizedBufferWriter writer(buffer);
        message.write(writer);
    }

    if (logFile_ != NULL) {
        boost::lock_guard<boost::mutex> lock(logMutex_);
        fprintf(logFile_, "Out: ");
        message.log(logFile_);
        fflush(logFile_);
    }

    boost::lock_guard<boost::mutex> lock(writeMutex_);
    if (fwrite(buffer.buffer(), buffer.size(), 1, outputWriteFile_) != 1) {
        std::cerr << "Server::send: Failed to send message, aborting.\n";
        abort();
    }
    if (fflush(outputWriteFile_) != 0) {
        std::cerr << "Server::send: Failed to flush message, aborting.\n";
        abort();
    }
}

boost::optional<const Export&> Server::lookup(const std::string& name) {
    std::map<std::string, const Export>::iterator it = exports_.find(name);
    if (it == exports_.end()) {
        return boost::none;
    } else {
        return it->second;
    }
}

void Server::registerServerRequest(const request_id_t requestId, ServerRequest& request) {
    boost::lock_guard<boost::mutex> lock(serverRequestsMutex_);
    serverRequests_[requestId] = &request;
}

void Server::unregisterServerRequest(const request_id_t requestId) {
    boost::lock_guard<boost::mutex> lock(serverRequestsMutex_);
    serverRequests_.erase(requestId);
}

boost::optional<request_id_t> Server::registerThreadRequest(const request_id_t requestId) {
    boost::lock_guard<boost::mutex> lock(threadRequestsMutex_);
    boost::optional<request_id_t> parentId;
    std::list<request_id_t>& thisThreadRequests = threadRequests_[boost::this_thread::get_id()];
    if (thisThreadRequests.empty()) {
        parentId = boost::none;
    } else {
        parentId = *thisThreadRequests.begin();
    }
    thisThreadRequests.push_front(requestId);
    return parentId;
}

void Server::unregisterThreadRequest(const request_id_t requestId) {
    boost::lock_guard<boost::mutex> lock(threadRequestsMutex_);
    const boost::thread::id threadId = boost::this_thread::get_id();
    std::list<request_id_t>& thisThreadRequests = threadRequests_[threadId];
    const request_id_t head = *thisThreadRequests.begin();
    if (head != requestId) {
        std::cerr << "Server::unregisterThreadRequest: Want to unregister request "
                  << requestId << ", but is currently on request " << head << ".  Aborting.\n";
        abort();
    }
    if (thisThreadRequests.size() == 1) {
        threadRequests_.erase(threadId);
    } else {
        thisThreadRequests.pop_front();
    }
}

ServerRequestRegistrationGuard::ServerRequestRegistrationGuard(
    Server& server,
    request_id_t requestId,
    ServerRequest& request) :
    server_(server), requestId_(requestId) {
    server_.registerServerRequest(requestId_, request);
}

ServerRequestRegistrationGuard::~ServerRequestRegistrationGuard() {
    server_.unregisterServerRequest(requestId_);
}


ThreadRequestRegistrationGuard::ThreadRequestRegistrationGuard(
    Server& server, const request_id_t requestId) :
    server_(server),
    requestId_(requestId),
    parentRequestId_(server_.registerThreadRequest(requestId_)) {}

ThreadRequestRegistrationGuard::~ThreadRequestRegistrationGuard() {
    server_.unregisterThreadRequest(requestId_);
}

boost::optional<request_id_t> ThreadRequestRegistrationGuard::getParentRequestId() const {
    return parentRequestId_;
}

Listener::Listener(Server& server, FILE* inputFile) :
    server_(server), inputFile_(inputFile) {}

void Listener::operator()() {
    SizedBuffer buffer;

    while (true) {
        size_t messageSize;
        if (fread(&messageSize, sizeof(size_t), 1, inputFile_) != 1) {
            std::cerr << "Listener: Failed to read size from pipe.  Aborting.\n";
            abort();
        }

        buffer.ensureSize(messageSize);
        if (fread(buffer.buffer(), messageSize, 1, inputFile_) != 1) {
            std::cerr << "Listener: Failed to read message from pipe.  Aborting.\n";
            abort();
        }

        SizedBufferReader reader(buffer);
        const Message* const message = Message::read(reader);
        FILE* const logFile = server_.getLogFile();
        if (logFile != NULL) {
            boost::lock_guard<boost::mutex> lock(server_.getLogMutex());
            fprintf(logFile, "In : ");
            message->log(logFile);
            fflush(logFile);
        }

        boost::optional<ServerRequest*> const maybeServerRequest =
            server_.findServerRequest(message->getRequestId());
        MVar<const Message*>* runnerVar;
        if (maybeServerRequest == boost::none) {
            runnerVar = listenerVar_.take();
        } else {
            ServerRequest& serverRequest = **maybeServerRequest;
            runnerVar = &serverRequest.getResponseVar();
        }

        runnerVar->put(message);
    }
}

Runner::Runner(Server& server, MVar<MVar<const Message*>*>& listenerVar) :
    server_(server), listenerVar_(listenerVar) {}

void Runner::operator()() {
    while (true) {
        listenerVar_.put(&messageVar_);
        const scoped_ptr<const Message> message(messageVar_.take());
        message->executeOn(server_);
    }
}

} // namespace cppop
