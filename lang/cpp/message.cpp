#include "message.h"

#include <cstring>
#include "server.h"

namespace cppop {

Message* Message::read(SizedBufferReader& reader) {
    const size_t commandSize = 4;
    const char* const command = reader.read(commandSize);
    Message* message;

    if (memcmp("CALL", command, commandSize) == 0) {
        message = new CallMessage(new SizedBuffer());
    } else if (memcmp("RETN", command, commandSize) == 0) {
        message = new RetnMessage(new SizedBuffer());
    } else if (memcmp("CALC", command, commandSize) == 0) {
        message = new CalcMessage(new SizedBuffer());
    } else {
        return NULL;
    }

    message->readContents(reader);
    return message;
}

void Message::write(SizedBufferWriter& writer) const {
    const size_t sizeOfs = writer.alloc(sizeof(size_t));
    const size_t initialSize = writer.writtenSize();
    writeContents(writer);
    const size_t finalSize = writer.writtenSize();
    *(size_t*)writer.buffer().at(sizeOfs) = finalSize - initialSize;
}

void CalcMessage::executeOn(Server& server) const {
    server.execute(*this);
}

void CalcMessage::log(FILE* file) const {
    fprintf(file,
        "CALC, rid=" PRI_REQUEST_ID ", rpid=" PRI_REQUEST_ID ", cid=" PRI_CALLBACK_ID
        ", %zu-byte body:\n",
        requestId_, parentRequestId_.get_value_or(0), callbackId_, body_->size());
    hexDump(file, body_->buffer(), body_->size());
}

void CalcMessage::readContents(SizedBufferReader& reader) {
    reader.readLiteralTo(&requestId_);

    const request_id_t parentRequestId = reader.readLiteral<request_id_t>();
    if (parentRequestId == 0) {
        parentRequestId_ = boost::none;
    } else {
        parentRequestId_ = parentRequestId;
    }

    reader.readLiteralTo(&callbackId_);

    const size_t bodySize = reader.remainingBytes();
    body_->ensureSize(bodySize);
    reader.readTo(body_->buffer(), bodySize);
}

void CalcMessage::writeContents(SizedBufferWriter& writer) const {
    writer.writeLiteral('C');
    writer.writeLiteral('A');
    writer.writeLiteral('L');
    writer.writeLiteral('C');
    writer.writeLiteral(requestId_);
    writer.writeLiteral(parentRequestId_.get_value_or(0));
    writer.writeLiteral(callbackId_);
    writer.write(*body_);
}

void CallMessage::executeOn(Server& server) const {
    server.execute(*this);
}

void CallMessage::log(FILE* file) const {
    fprintf(file,
        "CALL, rid=" PRI_REQUEST_ID ", rpid=" PRI_REQUEST_ID ", name=%s, %zu-byte body:\n",
        requestId_, parentRequestId_.get_value_or(0), functionName_.c_str(), body_->size());
    hexDump(file, body_->buffer(), body_->size());
}

void CallMessage::readContents(SizedBufferReader& reader) {
    reader.readLiteralTo(&requestId_);

    const request_id_t parentRequestId = reader.readLiteral<request_id_t>();
    if (parentRequestId == 0) {
        parentRequestId_ = boost::none;
    } else {
        parentRequestId_ = parentRequestId;
    }

    const char* name = reader.cursor();
    size_t nameSize = 0;
    while (*name++ != '\0') {
        ++nameSize;
    }
    functionName_ = std::string(reader.read(nameSize + 1));

    const size_t bodySize = reader.remainingBytes();
    body_->ensureSize(bodySize);
    reader.readTo(body_->buffer(), bodySize);
}

void CallMessage::writeContents(SizedBufferWriter& writer) const {
    writer.writeLiteral('C');
    writer.writeLiteral('A');
    writer.writeLiteral('L');
    writer.writeLiteral('L');
    writer.writeLiteral(requestId_);
    writer.writeLiteral(parentRequestId_.get_value_or(0));
    writer.write(functionName_.c_str(), functionName_.length());
    writer.writeLiteral('\0');
    writer.write(*body_);
}

void RetnMessage::executeOn(Server& server) const {
    server.execute(*this);
}

void RetnMessage::log(FILE* file) const {
    fprintf(file,
        "RETN, rid=" PRI_REQUEST_ID ", %zu-byte body:\n",
        requestId_, body_->size());
    hexDump(file, body_->buffer(), body_->size());
}

void RetnMessage::readContents(SizedBufferReader& reader) {
    reader.readLiteralTo(&requestId_);
    const size_t bodySize = reader.remainingBytes();
    body_->ensureSize(bodySize);
    reader.readTo(body_->buffer(), bodySize);
}

void RetnMessage::writeContents(SizedBufferWriter& writer) const {
    writer.writeLiteral('R');
    writer.writeLiteral('E');
    writer.writeLiteral('T');
    writer.writeLiteral('N');
    writer.writeLiteral(requestId_);
    writer.write(*body_);
}

}
