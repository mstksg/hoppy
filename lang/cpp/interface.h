#ifndef INTERFACE_H
#define INTERFACE_H

#include <map>
#include <string>

#include "buffers.h"

namespace cppop {

class Interface;

typedef const Interface* (*InterfaceFn)();

typedef void (*ExportFn)(BufferReader*, WritableBuffer*);

// Export is copyable!
class Export {
public:
    Export(
        const Interface* interface,
        const std::string& name,
        ExportFn exportFn) :
        interface_(interface), name_(name), exportFn_(exportFn) {}

    const Interface* interface() const { return interface_; }
    const std::string& name() const { return name_; }
    ExportFn exportFn() const { return exportFn_; }

private:
    const Interface* const interface_;
    const std::string name_;
    const ExportFn exportFn_;
};

typedef std::map<std::string, Export> ExportMap;

class Interface {
public:
    explicit Interface(const std::string& name);

    const std::string& name() const { return name_; }

    // Returns true on success.
    bool define(const std::string& name, ExportFn exportFn);

    // Prevents any further mutations to the interface.  Returns true on
    // success.
    bool finish();

    // May return null.  The returned pointer is valid as long as the interface
    // is not modified.
    Export* lookup(const std::string& name);

    const ExportMap& exportMap() const;

private:
    Interface(const Interface&);

    const std::string& name_;
    ExportMap exportMap_;
    bool finished_;
};

}

#endif // INTERFACE_H
