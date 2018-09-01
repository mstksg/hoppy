# Hoppy Changelog

## Unreleased

- Generated bindings now require GHC 7.10 or newer (released Apr 2015), and no
  longer use the CPP extension (issue #35).

- Generated bindings now have the MonoLocalBinds extension enabled to address
  the -Wsimplifiable-class-constraints warning in GHC 8.2+ (issue #36).

## hoppy-runtime-0.5.1 (2018-08-25)

- Bump to support Cabal 2.2.

## hoppy-generator-0.5.1 (2018-08-06)

- Added the ForeignFunctionInterface extension explicitly to generated bindings,
  because of a report at https://gitlab.com/khumba/qtah/issues/30 that it isn't
  always enabled by default.

- Added compatibility with GHC 8.4 with respect to the Semigroup Monoid Proposal
  (https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid), thanks
  @locallycompact.

## *-0.5.0 (2018-06-05)

Big thanks to Masahiro Sakai for his contributions to this release, which
include all of the following.

- Fixed exception handlers declared on functions incorrectly being sorted, when
  the order should be taken as is, because it affects the order in which 'catch'
  blocks appear in generated C++ code (issue #27).

- Fixed Haskell exception upcasting code, which caused build errors for
  generated code (MR !3).

- Fixed building of the example program on macOS (MR !4).

- Fixed NUL character handling in the conversions between Haskell's `String` and
  C++'s `std::string`, each of which can hold NUL characters (MR !10).

- Fixed the bindings for `std::set` to account for the fact that the C++
  standard allows `set::iterator` and `set::const_iterator` to be different
  types, and added bindings for `std::unordered_map` and `std::unordered_set`
  (MR !9).

- Changes to support 'cabal new-build' and 'new-repl' (MRs !7, !8).

## *-0.4.0 (2018-01-27)

- Fixed a const-safety bug with methods.  The C++ generator was always using
  non-const this pointers when writing gateway functions for methods, even when
  a method was marked as const.  This meant that it was possible to use
  "mkConstMethod" on a non-const method and pass in const objects (which would
  be implicitly cast to non-const).

  This may cause breakage when building your bindings, but only does so if you
  have mistakenly labelled non-const methods as const.  std::string::at() was
  affected by this and has been corrected.

- Fixed some incorrect typeclass names in the user's guide.

## hoppy-generator-0.3.4, hoppy-runtime-0.3.2 (2017-12-30)

- Added a means for using a custom `shared_ptr` implementation via
  `interfaceSetSharedPtr`.  Hoppy normally uses `std::shared_ptr` in its
  implementation of callbacks, but this isn't always available (e.g. with older
  compilers).

- Added support for Cabal 2.0.

- Bug fixes for bitspaces:

  - Negative numeric values are now supported, as they are for enums.

  - There was a duplicate typeclass instance being generated when a bitspace's
    Haskell-side Haskell and C numeric types were the same.

- Bug fixes for exception handling:

  - The generated Haskell code for functions that throw and also need
    Haskell-side return value conversion wouldn't compile (the
    internalHandleExceptions call was placed at the wrong point).

  - There was a null pointer dereference in C++ coming back from throwing
    callbacks that return refT or objT.

  - Generated code used a mix of prepended and appended exception arguments for
    callbacks (everywhere prepended except for callbackToTFn).  Now callbacks
    consistently append those arguments, as is done for throwing C++ functions.

## hoppy-generator-0.3.3, hoppy-runtime-0.3.1, hoppy-docs-0.3.2 (2017-06-08)

- Added default Setup.hs implementations for gateway packages, to encapsulate
  the magic that is needed to make bindings work (issue #13).  See
  Foreign.Hoppy.Setup in hoppy-runtime and the example/ directory that uses it.

- Callback functors are now default constructable, so that callbacks in C++ can
  be optional.  Like std::shared_ptr, casting to bool determines whether the
  callback is populated and may be called.  It is not valid to invoke a
  default-constructed callback.

## hoppy-generator-0.3.2, hoppy-docs-0.3.1 (2017-02-26)

- Added a proper tutorial to the user's guide, and improved various bits of
  documentation in the generator.

- Added a default `main` implementation to the generator for bindings to use,
  instead of having to write their own.

## hoppy-generator-0.3.1 (2017-02-18)

- Fixed an infinite loop in the C++ generator and a mistake in the Haskell
  generator when using `toGcT . objT` in a callback argument (issue #22).

- Dependency bump to support directory-1.3 in GHC 8.0.2.

## *-0.3.0 (2016-12-16)

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

## *-0.2.1 (2016-10-01)

- Bumped the upper version of the base dependency to 5; base releases often.

## *-0.2.0 (2016-06-29)

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

## *-0.1.0 (2016-02-08)

- Initial release.
