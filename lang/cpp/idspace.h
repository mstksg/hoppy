#ifndef IDSPACE_H
#define IDSPACE_H

#include <boost/thread.hpp>
#include <boost/noncopyable.hpp>
#include <cstdlib>
#include <iostream>
#include <set>
#include <string>

template <typename I>
class IdSpace : private boost::noncopyable {
public:
    IdSpace(const std::string& name, I base, I step);

    const std::string& name() const;

    I request();

    void release(I id);

private:
    const std::string name_;
    const I base_;
    const I step_;
    std::set<I> usedIds_;
    boost::mutex mutex_;
};

template <typename I>
IdSpace<I>::IdSpace(const std::string& name, I base, I step) :
    name_(name), base_(base), step_(step) {
    if (step_ == 0) {
        std::cerr << "Space \"" << name_ << "\" cannot have a step of zero!\n";
        abort();
    }
}

template <typename I>
const std::string& IdSpace<I>::name() const {
    return name_;
}

template <typename I>
I IdSpace<I>::request() {
    boost::lock_guard<boost::mutex> lock(mutex_);
    I id;
    if (usedIds_.empty()) {
        id = base_;
    } else if (step_ > 0) {
        id = *usedIds_.rbegin() + step_;
    } else {
        id = *usedIds_.begin() + step_;
    }
    usedIds_.insert(id);
    return id;
}

template <typename I>
void IdSpace<I>::release(I id) {
    boost::lock_guard<boost::mutex> lock(mutex_);
    typename std::set<I>::iterator it = usedIds_.find(id);
    if (it == usedIds_.end()) {
        std::cerr << "Trying to free unused id " << id << " in space \""
                  << name_ << "\".  Ignoring.\n";
    } else {
        usedIds_.erase(it);
    }
}

#endif // IDSPACE_H
