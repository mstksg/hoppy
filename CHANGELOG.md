# Hoppy Changelog

## Unreleased 0.3.0

- Added support for catching C++ exceptions in Haskell (issue #10).

- Added support for customizing the prefixes of class methods and constructors,
  as well as enum and bitspace values, so that we can simplify the Qtah
  generator and make using Qtah's enums and bitspaces less painful on the
  fingers (https://gitlab.com/khumba/qtah/issues/10).

  Consequently, Haskell name generation functions are now monadic, since
  generated Haskell modules now import each other qualified (but this simplifies
  the Haskell generator some since cross-module dependencies are now imported
  implicitly).

- Added addenda to modules, to provide a place to inject custom code at the
  module level (issue #11).

- Added the ability to enable custom Haskell language extensions for generated
  modules.

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
