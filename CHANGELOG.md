# Hoppy Changelog

## Unreleased
### Added

- Added foreign garbage collector support (issue #5).

- Classes can now be set so that returning an `objT cls` is equivalent to
  returning `objToHeapT cls` or `toGcT (objT cls)`.

- Bitspace parameters to C++ functions now accept any type in the bitspace's
  typeclass, rather than just the bitspace type (so integers can be passed
  directly).

### Changed

- Major API change for specifying C++ types.  A set of bindings replaces the
  data constructors of `Type`.  This is to make it easier to create new types in
  the future.

- `intT`, `floatT`, and `doubleT` now use `Int`, `Float`, and `Double` as their
  Haskell types, rather than the awkward `CInt`, `CFloat`, and `CDouble`.

### Fixed

- Small fixes to the user guide and unit tests.

## 0.1.0 (2016-02-08)

- Initial release.
