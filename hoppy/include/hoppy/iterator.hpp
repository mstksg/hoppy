#ifndef HOPPY_ITERATOR_HPP
#define HOPPY_ITERATOR_HPP

#include <iterator>

namespace hoppy {
namespace iterator {

template <typename Iter, typename T>
T& put(Iter* self, const T& value) {
    return (**self = value);
}

}  // namespace iterator
}  // namespace hoppy

#endif
