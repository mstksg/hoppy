#ifndef MESSAGE_H
#define MESSAGE_H

#include <boost/noncopyable.hpp>
#include <cstdio>
#include "buffer.h"
#include "common.h"

namespace cppop {

class Server;

class Message : private boost::noncopyable {
public:
    virtual ~Message() {}

    // Returns null if we can't read a valid message.
    static Message* read(SizedBufferReader& reader);

    void write(SizedBufferWriter& writer) const;

    virtual void executeOn(Server& server) const = 0;

    virtual void log(FILE* file) const = 0;

protected:
    virtual void readContents(SizedBufferReader& reader) = 0;

    virtual void writeContents(SizedBufferWriter& writer) const = 0;
};

class CalcMessage : public Message {
public:
    explicit CalcMessage(SizedBuffer* body) :
        requestId_(0), parentRequestId_(0), callbackId_(0), body_(body) {}
    virtual ~CalcMessage() {}

    request_id_t getRequestId() const { return requestId_; }
    void setRequestId(request_id_t requestId) { requestId_ = requestId; }
    request_id_t getParentRequestId() const { return parentRequestId_; }
    void setParentRequestId(request_id_t parentRequestId) { parentRequestId_ = parentRequestId; }
    callback_id_t getCallbackId() const { return callbackId_; }
    void setCallbackId(callback_id_t callbackId) { callbackId_ = callbackId; }
    SizedBuffer& getBody() const { return *body_; }

    virtual void executeOn(Server& server) const;

    virtual void log(FILE* file) const;

protected:
    virtual void readContents(SizedBufferReader& reader);

    virtual void writeContents(SizedBufferWriter& writer) const;

private:
    request_id_t requestId_;
    request_id_t parentRequestId_;
    callback_id_t callbackId_;
    scoped_ptr<SizedBuffer> body_;
};

class CallMessage : public Message {
public:
    explicit CallMessage(SizedBuffer* body) :
        requestId_(0), parentRequestId_(0), functionName_(), body_(body) {}
    virtual ~CallMessage() {}

    request_id_t getRequestId() const { return requestId_; }
    void setRequestId(request_id_t requestId) { requestId_ = requestId; }
    request_id_t getParentRequestId() const { return parentRequestId_; }
    void setParentRequestId(request_id_t parentRequestId) { parentRequestId_ = parentRequestId; }
    std::string getFunctionName() const { return functionName_; }
    void setFunctionName(std::string functionName) { functionName_ = functionName; }
    SizedBuffer& getBody() const { return *body_; }

    virtual void executeOn(Server& server) const;

    virtual void log(FILE* file) const;

protected:
    virtual void readContents(SizedBufferReader& reader);

    virtual void writeContents(SizedBufferWriter& writer) const;

private:
    request_id_t requestId_;
    request_id_t parentRequestId_;
    std::string functionName_;
    scoped_ptr<SizedBuffer> body_;
};

class RetnMessage : public Message {
public:
    explicit RetnMessage(SizedBuffer* body) :
        requestId_(0), body_(body) {}
    virtual ~RetnMessage() {}

    request_id_t getRequestId() const { return requestId_; }
    void setRequestId(request_id_t requestId) { requestId_ = requestId; }
    SizedBuffer& getBody() const { return *body_; }

    virtual void executeOn(Server& server) const;

    virtual void log(FILE* file) const;

protected:
    virtual void readContents(SizedBufferReader& reader);

    virtual void writeContents(SizedBufferWriter& writer) const;

private:
    request_id_t requestId_;
    scoped_ptr<SizedBuffer> body_;
};

}

#endif // MESSAGE_H
