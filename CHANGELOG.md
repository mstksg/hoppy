# Hoppy Changelog

## Unreleased - 0.7.0

- [API change] There is a new `Scoped` boolean data type to indicate whether an
  enum is a scoped enum (e.g. `enum class`) or an unscoped enum (the old
  `enum`).  This type is now expected as an argument to `makeAutoEnum` rather
  than a raw boolean.  It turns out to be necessary to store this on enums
  themslves, so it makes sense to improve the `makeAutoEnum` parameter at the
  same time.

- Fix for autodetection of scoped enum values (issue #43).  For now,
  autodetection for scoped enums only works with C++11 and up.

## *-0.6.0 (2019-09-29)

This release is a significant refactoring of Hoppy, featuring many new features
and changes to its API.

- Compatibility with GHC 8.6, Cabal 2.4.

- [API change] C++ data types have been generalized, and you can now define your
  own kinds of entities to be exported, in parallel with enums, objects,
  variables, functions, etc.

  The core of this is the support is the `Exportable` typeclass, which defines
  the entry points that Hoppy will call to generate support code necessary on
  either side of a binding, if needed.  Rather than `Export` having a number of
  fixed data constructors, it's now an existential data type that wraps an
  `Exportable`, and can be created with the `toExport` function.

- Marshalling of values between C++ and Haskell has also been generalized and is
  now extensible to your own types or specific arguments, as you see fit
  (issue #9).

  The main entry point here is the `ConversionSpec` data type, which specifies
  C++ and Haskell types and the code generators necessary to convert values
  between them.  A `ConversionSpec` can be lifted to a `Type` with the function
  `manualT`, and various existing type functions (`intT`, `enumT`, etc.) are
  wrappers around this function.

  All of the built-in number types are now implemented via this mechanism, and
  you can create your own with `makeNumericType`.  The `Type` data type has been
  reduced to a handful of primitive types, and all of the nonessentials have
  been split out to use the new generic support.  This doesn't affect the use of
  the `typeT` functions.

  Classes and `objT` still remain to be unified with this new way.  For now,
  class conversions are still handled via `ClassConversion`.

- [API change] Enum support has been improved in many ways.

  First and most importantly, Hoppy can now compute the ordinal values for enum
  entries itself, by building a sample program with a provided compiler
  (issue #17).  Binding authors no longer need to determine these values, hard
  code them into their bindings, and hope they don't change.  These new-style
  enums can be created with `makeAutoEnum`.  Enums can still be created the old
  way, for example if it's not convenient to call a C++ compiler.  By default,
  g++ is assumed to be present and any manually-specified enum details will be
  verified by running the compiler.  Compilation can be disabled with
  `interfaceSetNoCompiler`.

  Secondly, as part of `makeAutoEnum`, enum entry names are easier to specify.
  You now just specify the C++ name of the entry, rather than splitting them up
  into words.

  Thirdly, previously Hoppy assumed that enums were backed by a C++ int, which
  is bad because this type can have both the wrong width, and the wrong
  signedness (issue #30).  As part of enum autodetection, this is computed
  automatically now.  For old-style enums, a data type can be provided manually.

  Fourthly, Hoppy used to generate `Bounded` and `Enum` instances for C++ enums
  in Haskell, and it no longer does (issue #7); instead, it generates an
  instance of a new typeclass `CppEnum`.  The instances generated were incorrect
  when the enum's numeric values weren't all sequential integers.  The instance
  generated for `Bounded` incorrectly used the first and last entries in the
  list provided to `makeEnum` (I suppose we could instead take the lowest and
  highest numeric value though).  For `Enum`, the instances used the default
  `succ` and `pred` instances of adding and subtracting one, which isn't
  guaranteed to work.  Also, both of these only work with `Int` (see the
  previous paragraph).  The new `CppEnum` typeclass only supports the bare
  minimum for now: `toCppEnum` and `fromCppEnum`.

  Fifthly, multiple enum entries having the same ordinal value is now supported.

  Sixthly, enums can now optionally represent values that aren't included in the
  binding's definition (issue #4).  This supports the case where a new enum
  value has been added by the C++ library but it hasn't been added to the
  bindings yet.  This is a choice for each enum: either the enum doesn't support
  unknown values, and Haskell will `error` if one is seen, or it will have an
  additional "unknown" data constructor that will have to be pattern-matched
  against, but `toCppEnum` will never error out.

  Seventhly, Haskell bindings for enums now get `Bits` instances.  This is on by
  default and can be disabled on a per-enum basis.

- [API change] Bitspaces are now removed.

  In case you didn't see this coming after the above: bitspaces were a hack in
  order to support QFlags in Qtah, and all of the logic necessary to support
  this in a more general fashion within Qtah itself has been added via the above
  points, so bitspaces are gone from Hoppy.

- [API change] Function parameters can now be named (issue #12).

  Names can now be attached to function parameters.  These names are used as
  type variable names and Haddock documentation generated code, so that binding
  users can see these names rather than just arg1, arg2, and so on, as before.

  Unfortunately, this causes some unpleasant API breakage.

  Parameter lists to `makeFn`, `mkMethod`, etc. are now generic.  They can
  either be `[Type]` as before, or `(~:)` can be used to attach names to types
  to give `[Parameter]`.  The breakage is that the empty list is now ambiguously
  typed, so a new `np = [] :: [Parameter]` value should be used instead.

- Initial support has been added for exported names to be overridable on a
  per-language basis, with data types in `Foreign.Hoppy.Generator.Overrides`.
  This is used in conjunction with `makeAutoEnum` enum entries, where this is
  the mechanism to provide a Haskell name different than the C++ name (see
  `enumAddEntryNameOverrides`).

  For all other entities (functions, variables, classes, etc.), you can just
  give the entity a different external name than its C++ name, and only one
  foreign language is supported, so this isn't necessary now, but these
  overrides may be more tightly integrated with `ExtName`s in the future.

- Generator binaries have new `--dump-ext-names` and `--dump-enums` options for
  displaying information for debugging purposes, suhc as enum information
  determined from the C++ compiler.

- `wchar_t` and `wstring` have been added to the standard library bindings.

- The Nix expressions, which had long been languishing, have been brought up to
  date, and now make use of Nix overlays.  The example now includes Nix
  expressions.

## hoppy-generator-0.5.2 (2018-09-01)

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
