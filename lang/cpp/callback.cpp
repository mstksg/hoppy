#include "callback.h"

#include "server.h"

namespace cppop {

Callback::Callback(Server& server, callback_id_t callbackId) :
    server_(server), callbackId_(callbackId) {}

void Callback::invoke(SizedBuffer* args, SizedBuffer& outResult) {
    server_.invokeCallback(callbackId_, args, outResult);
}

callback_id_t Callback::getId() const {
    return callbackId_;
}

}
