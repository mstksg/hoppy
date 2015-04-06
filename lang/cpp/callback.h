#ifndef CALLBACK_H
#define CALLBACK_H

#include "buffer.h"
#include "common.h"

namespace cppop {

class Server;

// A callback is a reference to a remote function.
//
// Callbacks are lightweight and should be passed by value.
class Callback {
public:
    Callback(Server& server, callback_id_t callbackId);

    // Takes ownership of the argument buffer.
    void invoke(SizedBuffer* args, SizedBuffer& outResult);

    callback_id_t getId() const;

private:
    Server& server_;
    const callback_id_t callbackId_;
};

}

#endif // CALLBACK_H
