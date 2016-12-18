# Hoppy Changelog

## Unreleased

- Fixed an infinite loop in the generator when using `toGcT . objT` in a
  callback argument.

## 0.3.0 (2016-12-16)

- [API change] Added support for customizing the prefixes of class methods and
  constructors, as well as enum and bitspace values, so that we can simplify the
  Qtah generator and make using Qtah's enums and bitspaces less painful on the
  fingers (https://gitlab.com/khumba/qtah/issues/10).

  Consequently, Haskell name generation functions are now monadic, since
  generated Haskell modules now import each other qualified (but this simplifies
  the Haskell generator some since cross-module dependencies are now imported
  implicitly).

- [API change] Merged constructors, methods, and properties into a single list
  when declaring classes, and simplifying down from the previous
  "makeClass ... [ctors] $ [methods] ++ mkProps [props]" to
  "makeClass ... [ctors..., methods..., props...]".

- [API change] Split class convertibility into convertibility in each direction
  (issue #15).  Now classSetConversionToGc and classSetConversionToHeap are not
  special; they're just implemented as a regular class conversion that only
  supports decoding.  This allows container classes whose conversions use
  'encode' and 'decode' to convert their elements, to contain convert-to-GC/heap
  objects as well now.

- Added support for class member variables.

- Added support for function pointers (issue #14).

- Added support for using C++ exceptions in Haskell (issue #10).  Exceptions are
  automatically passed between C++ and Haskell in gateway code that is specified
  to support them.  The main Haskell interface is via catchCpp and throwCpp.

- Added addenda to modules, to provide a place to inject custom code at the
  module level (issue #11).

- Added the ability to enable custom Haskell language extensions for generated
  modules.

## 0.2.1 (2016-10-01)

- Bumped the upper version of the base dependency to 5; base releases often.

## 0.2.0 (2016-06-29)

- Added foreign garbage collector support (issue #5).

- Classes can now be set so that returning an `objT cls` is equivalent to
  returning `objToHeapT cls` or `toGcT (objT cls)`.

- Bitspace parameters to C++ functions now accept any type in the bitspace's
  typeclass, rather than just the bitspace type (so integers can be passed
  directly).

- Major API change for specifying C++ types (issue #9).  A set of bindings
  replaces the data constructors of `Type`.  This is to make it easier to create
  new types in the future.

- `intT`, `floatT`, and `doubleT` now use `Int`, `Float`, and `Double` as their
  Haskell types, rather than the awkward `CInt`, `CFloat`, and `CDouble`
  (issue #3).

- Small fixes to the user guide and unit tests.

## 0.1.0 (2016-02-08)

- Initial release.
