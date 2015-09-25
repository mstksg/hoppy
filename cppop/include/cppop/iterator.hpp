#ifndef CPPOP_ITERATOR_HPP
#define CPPOP_ITERATOR_HPP

#include <iterator>

namespace cppop {
namespace iterator {

template <typename Iter, typename T>
T& put(Iter* self, const T& value) {
    return (**self = value);
}

}  // namespace iterator
}  // namespace cppop

#endif
