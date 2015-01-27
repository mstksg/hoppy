#include "driver.h"
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include "server.h"

namespace {

const unsigned int numServerThreadsDefault = 4;

}

namespace cppop {

int executeWithArgs(
    int argc,
    const char* const argv[],
    size_t interfaceCount,
    InterfaceFn interfaces[]) {
    std::string inPath;
    std::string outPath;
    std::string logPath;
    unsigned int numServerThreads = numServerThreadsDefault;

    std::vector<std::string> args;
    for (int i = 1; i < argc; ++i) {
        args.push_back(std::string(argv[i]));
    }

    const int argCount = args.size();
    for (int i = 0; i < argCount;) {
        const std::string& arg = args[i++];

        if (arg == "--in" || arg == "-i") {
            if (i == argCount) {
                std::cerr << "--in requries a FIFO path to read from.\n";
                return 1;
            }
            inPath = args[i++];

        } else if (arg == "--out" || arg == "-o") {
            if (i == argCount) {
                std::cerr << "--out requires a FIFO path to write to.\n";
                return 1;
            }
            outPath = args[i++];

        } else if (arg == "--log") {
            if (i == argCount) {
                std::cerr << "--log requires a file path to write to.\n";
                return 1;
            }
            logPath = args[i++];

        } else if (arg == "--threads") {
            if (i == argCount) {
                std::cerr << "--threads requires a positive numeric argument.\n";
                return 1;
            }
            if (sscanf(args[i++].c_str(), "%u", &numServerThreads) < 1 ||
                numServerThreads < 1) {
                std::cerr << "--threads requires a positive numeric argument.\n";
                return 1;
            }
        }
    }

    if (inPath.empty()) {
        std::cerr << "--in is required.\n";
        return 1;
    }
    if (outPath.empty()) {
        std::cerr << "--out is required.\n";
        return 1;
    }

    Server server;
    for (size_t i = 0; i < interfaceCount; ++i) {
        const Interface* interface = interfaces[i]();
        if (!server.addInterface(interface)) {
            // TODO Is interface freed at this point?  Are we allowed to
            // ask for its name?
            std::cerr << "Error registering interface \"" << interface->name()
                      << "\".\n";
            return 1;
        }
    }

    if (!server.start(inPath.c_str(), outPath.c_str(), logPath.c_str(), numServerThreads)) {
        std::cerr << "Error starting server.\n";
        return 1;
    }

    server.wait();

    return 0;
}

}
