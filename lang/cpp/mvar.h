#ifndef MVAR_H
#define MVAR_H

#include <boost/noncopyable.hpp>
#include <boost/thread/thread.hpp>

namespace cppop {

template <typename T>
class MVar : private boost::noncopyable {
public:
    MVar() : full_(false) {}
    MVar(T value) : full_(true), value_(value) {}

    T take() {
        boost::unique_lock<boost::mutex> lock(mutex_);
        while (!full_) {
            condNowFull_.wait(lock);
        }
        T value = value_;
        full_ = false;
        lock.unlock();
        condNowEmpty_.notify_all();
        return value;
    }

    void put(T value) {
        boost::unique_lock<boost::mutex> lock(mutex_);
        while (full_) {
            condNowEmpty_.wait(lock);
        }
        value_ = value;
        full_ = true;
        lock.unlock();
        condNowFull_.notify_all();
    }

private:
    bool full_;
    T value_;

    boost::mutex mutex_;
    boost::condition_variable condNowEmpty_;
    boost::condition_variable condNowFull_;
};

}

#endif // MVAR_H
