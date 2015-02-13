#include "interface.h"

#include <iostream>
#include <string>
#include "buffer.h"

namespace cppop {

Interface::Interface(const std::string& name) :
    name_(name),
    finished_(false) {}

bool Interface::define(const std::string& name, ExportFn exportFn) {
    if (finished_) {
        std::cerr << "Interface::define: Trying to add methods to a finished "
                  << "interface \"" << name_ << "\".";
        return false;
    }

    ExportMap::iterator existingEntry = exportMap_.find(name);
    if (existingEntry != exportMap_.end()) {
        std::cerr << "Interface::define: Interface \"" << name_
                  << "exports name \"" << name << " multiple times.\n";
        return false;
    }

    exportMap_.insert(std::make_pair(name, Export(*this, name, exportFn)));
    return true;
}

bool Interface::finish() {
    if (finished_) {
        return false;
    }
    finished_ = true;
    return true;
}

boost::optional<Export&> Interface::lookup(const std::string &name) {
    ExportMap::iterator it = exportMap_.find(name);
    if (it == exportMap_.end()) {
        return boost::none;
    } else {
        return it->second;
    }
}

const ExportMap& Interface::exportMap() const {
    return exportMap_;
}

}
