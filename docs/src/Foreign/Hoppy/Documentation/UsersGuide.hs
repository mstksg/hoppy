-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The Hoppy User's Guide
module Foreign.Hoppy.Documentation.UsersGuide (
  -- * Overview
  -- $overview

  -- * Getting started
  -- $getting-started

  -- ** Project setup
  -- $getting-started-project-setup

  -- ** Concepts
  -- $getting-started-concepts

  -- * Generators
  -- $generators

  -- ** C++
  -- $generators-cpp

  -- *** Module structure
  -- $generators-cpp-module-structure

  -- *** Object passing
  -- $generators-cpp-object-passing

  -- *** Callbacks
  -- $generators-cpp-callbacks

  -- ** Haskell
  -- $generators-hs

  -- *** Module structure
  -- $generators-hs-module-structure

  -- **** Variable exports
  -- $generators-hs-module-structure-variables

  -- **** Enum exports
  -- $generators-hs-module-structure-enums

  -- **** Bitspace exports
  -- $generators-hs-module-structure-bitspaces

  -- **** Function exports
  -- $generators-hs-module-structure-functions

  -- **** Callback exports
  -- $generators-hs-module-structure-callbacks

  -- **** Class exports
  -- $generators-hs-module-structure-classes

  -- *** Module dependencies
  -- $generators-hs-module-dependencies

  -- *** Object passing
  -- $generators-hs-object-passing
  ) where

import Data.Bits (Bits)
import Foreign.C (CInt)
import Foreign.Hoppy.Generator.Language.Haskell.General
import Foreign.Hoppy.Generator.Main
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Version
import Foreign.Hoppy.Runtime
import Foreign.Ptr (castPtr)
import Language.Haskell.Syntax (HsType)
import System.IO.Unsafe (unsafePerformIO)

{- $overview

Hoppy is a foreign function interface (FFI) generator for interfacing Haskell
with C++.  It lets developers specify C++ interfaces in pure Haskell, and
generates code to expose that functionality to Haskell.  Hoppy is made up of a
few different packages that provide interface definition data structures and
code generators, some runtime support for Haskell bindings, and interface
definitions for the C++ standard library.

Bindings using Hoppy have three parts:

- A Haskell generator program (in @\/generator@) that knows the interface
definition and generates code for the next two parts.

- A C++ library (in @\/cpp@) that gets compiled into a shared object containing
the C++ half of the bindings.

- A Haskell library (in @\/hs@) that links against the C++ library and exposes
the bindings.

The path names are suggested subdirectories of a project, and are used in this
document, but are not required.  Only the latter two items need to be packaged
and distributed to users of the binding (plus Hoppy itself which is a dependency
of the generated bindings).

-}
{- $getting-started

This section is for getting out of the gate running.

-}
{- $getting-started-project-setup

To bind to a C++ library, first the binding author writes a generator program
(@\/generator@) in Haskell.  This program should define the complete C++
interface that is to be exposed.  The binding author also writes a @Main.hs@
file for invoking the generator (usually deferring to
"Foreign.Hoppy.Generator.Main").  If necessary, she should also write wrappers
for C++ things that she doesn't want to expose directly (in @\/cpp@).

Then, her build process should perform the following steps:

1. Compile the generator (@\/generator@).

2. Run the generator to create the C++ and Haskell sides of the bindings in
@\/cpp@ and @\/hs\/src@ respectively.  See the documentation for 'run' for how
to invoke a generator.

3. Compile the C++ side of the bindings into a shared object.  Make sure to
compile with the version of the C++ standard that matches what the generator was
run with (see 'activeCppVersion').

4. Compile the Haskell side of the bindings, linking with the C++ library.

For this last step, the @.cabal@ file in @\/hs@ should have

> extra-libraries: foo

to link against a shared object @libfoo.so@.  If this library is not on the
system's library search path, then she will need to specify
@--extra-lib-dirs=...\/cpp@ to the @cabal configure@ for @\/hs@.

The unit tests provide some simple examples of this setup.

-}
{- $getting-started-concepts

A complete C++ API is specified using Haskell data structures in
"Foreign.Hoppy.Generator.Spec".  At the top level is the 'Interface' type.  An
interface contains 'Module's which correspond to a portion of functionality of
the interface (collections of classes, functions, files, etc.).  Functionality
can be grouped arbitrarily into modules and doesn't have to follow the structure
of existing C++ files.  Modules contain 'Export's which refer to concrete things
that provide bindings.  Binding definitions take advantage of Haskell's
laziness, and can be highly circular, a simple case being a class that includes
a method that makes use of the class in its parameter or return types.

Each export has an /external name/ that uniquely identifies it within an
interface.  This name can be different from the name of the C++ the export is
referring to.  An external name is munged by the code generators and must be a
valid identifier in all languages a set of bindings will use, so it is
restricted to characters in the range @[a-zA-Z0-9_]@, and must start with an
alphabetic character.  Character case in external names will be preserved as
much as possible in generated code, although case conversions are sometimes
necessary (e.g. Haskell requiring identifiers to begin with upper or lower case
characters).

C++ bindings for exportable things usually need @#include@s in order to access
those things.  This is done with 'Include' and 'Reqs'.  All exportable things
have an instance of 'HasUseReqs' and 'addReqIncludes' can be used to add
includes.

C++ identifiers are represented by the 'Identifier' data type and support basic
template syntax (no metaprogramming).

All C++ types are represented with the 'Type' data type, which includes
primitive numeric types, object types, function types, @void@, the const
qualifier, etc.  When passing values back and forth between C++ and Haskell,
generally, primitive types are converted to equivalent types on both ends, and
pointer types in C++ are represented by corresponding pointer types in Haskell.

This is not true for raw object types (not pointers or references, just the
by-value object).  When an object is taken or returned by value, this typically
indicates a lightweight object that is easy to copy, so Hoppy will convert the
object to a native Haskell object specified in the class's binding definition.
Using an object type directly is only allowed if a native Haskell type is
defined.  See 'ClassConversions' for more on object conversions.

-}
{- $generators

This section describes the behaviour of the code generators.  The code
generators live at @Foreign.Hoppy.Generator.Language.\<language>@.  The
top-level module for a language is internal to Hoppy and contains the bulk of
the generator.  @General@ submodules expose functionality that can control
generator behaviour.

-}
{- $generators-cpp

The C++ code generator generates C++ bindings that other languages' bindings
will link against.  This generator lives in
"Foreign.Hoppy.Generator.Language.Cpp", with public parts in
"Foreign.Hoppy.Generator.Language.Cpp.General".

-}
{- $generators-cpp-module-structure

Generated modules consist of a source and a header file.  The source file
contains all of the bindings for foreign languages to make use of.  The header
file contains things that may be depended on from other generated modules.
Currently this consists only of generated callback classes.

Cycles between generated C++ modules are not supported.  This can currently only
happen because of @#include@ cycles involving callbacks, since callbacks are the
only 'Export's that can be referenced by other generated C++ code.

-}
{- $generators-cpp-object-passing

> data Type = ... | TPtr Type | TRef Type | TObj Class | TConst

We consider all of the following cases as passing an object, both into and out
of C++, and independently, as an argument and as a return value:

1. @'TObj' _@
2. @'TRef' ('TConst' ('TObj' _))@
3. @'TRef' ('TObj' _)@
4. @'TPtr' ('TConst' ('TObj' _))@
5. @'TPtr' ('TObj' _)@

The first is equivalent to @'TConst' ('TObj' _)@.  When passing an argument from
a foreign language to C++, the first two are equivalent, and it's recommended to
use the first, shorter form (@T@ and @const T&@ are functionally equivalent in
C++, and are the same as far as what values foreign bindings will accept).

When passing any of the above types as an argument in either direction, an
object is passed between C++ and a foreign language via a pointer.  Cases 1, 2,
and 4 are passed as const pointers.  For a foreign language passing a @'TObj' _@
to C++, this means converting a foreign value to a temporary C++ object.
Passing a @'TObj' _@ argument into or out of C++, the caller always owns the
object.

When returning an object, again, pointers are always what is passed across the
language boundary in either direction.  Returning a @'TObj' _@ transfers
ownership: a C++ function returning a @'TObj' _@ will copy the object to the
heap, and return a pointer to the object which the caller owns; a callback
returning a @'TObj' _@ will internally create a C++ object from a foreign value,
and hand that object off to the C++ side (which will return it and free the
temporary).

-}
{- $generators-cpp-callbacks

> data Type = ... | TCallback Callback
>
> data Callback = Callback ExtName [Type] Type ...  -- Parameter and return types.

We want to call some foreign code from C++.  What C++ type do we associate with
such an entry point?  (Both the C++ and foreign sides of the callback will need
to perform en-\/decoding of arguments\/return values.)

__Function pointer:__ Create a function pointer to a foreign wrapper which does
en-/decoding on the foreign side.  But then we need to wrap this in a C++
function (pointer) which does the C++-side conversions.  Function pointers can't
close over variables, so this doesn't work.

__C++ functor:__ Create a class G that takes a foreign function pointer and
implements @operator()@, performing the necessary conversions around invoking
the pointer.  In the event that the function pointer is dynamically allocated
(as in Haskell), then this class also ties the lifetime of the function pointer
to the lifetime of the class.  But this would cause problems for passing this
object around by value, so instead we make G non-copyable and non-assignable,
allocate our G instance on the heap, and create a second class F that holds a
@shared_ptr\<G>@ and whose @operator()@ calls through to G.

This way, the existance of the F and G objects are invisible to the foreign
language, and (for now) passing these callbacks back to the foreign language is
not supported.

When a binding is declared to take a callback type, the generated foreign side
of the binding will take a foreign function (the callback) with foreign-side
types, and use a function (Haskell: callbackName) generated for the callback
type to wrap the callback in a foreign function that does argument decoding and
return value encoding: this wrapped function will have C-side types.  The
binding will then create a G object (above) for this wrapped function (Haskell:
using callbackName'), and pass a G pointer into the C side of the binding.  The
binding will decode this C pointer by wrapping it in a temporary F object, and
passing that to the C++ function.  The C++ code is free to copy this F object as
much as it likes.  If it doesn't store a copy somewhere before returning, then
the when the temporary F object is destructed, the G object will get deleted.

-}
{- $generators-hs

The Haskell code generator lives in "Foreign.Hoppy.Generator.Language.Haskell",
with public parts in "Foreign.Hoppy.Generator.Language.Haskell.General".

Central to generated Haskell bindings is the idea of type sidedness and the
'HsTypeSide' enum.  When a value is passed to or from C++, it needs to be
converted so that the receiving language knows what to do with it.  The C++ side
of bindings just exchanges C types across the language boundary and does not do
conversions, so it is up to the Haskell side to do so.  Internally, the Haskell
generator refers to types exchanged with C++ as /C-side/ types, and types the
bindings exchange with user Haskell code as /Haskell-side/ types.  These are
both Haskell types!  The terminology is overlapped a bit but generally, /type/
or /C++ type/ refers to a 'Type', and in the context of the Haskell generator,
/C-side/ or /Haskell-side/ apply to a 'HsType', calculated from a 'Type' and a
'HsTypeSide' using 'cppTypeToHsTypeAndUse'.  For many primitive C++ types, the
C-side and Haskell-side types are the same.

-}
{- $generators-hs-module-structure

The result of generating a Hoppy module is a single Haskell module that contains
bindings for everything exported from the Hoppy module.  The Haskell module name
is the concatenation of the interface's 'interfaceHaskellModuleBase' and the
module's 'moduleHaskellName'.

The contents of the module depends on the what 'Export's the module has.

-}
{- $generators-hs-module-structure-variables

A 'Variable' is exposed in Haskell as a getter function and a setter function.
For a variable with external name @foo@ with Haskell-side type @Bar@, the
following functions are created:

> foo_get :: IO Bar
> foo_set :: Bar -> IO ()

-}
{- $generators-hs-module-structure-enums

A 'CppEnum' is exposed in Haskell as an enumerable data type.  For an enum
defined as follows:

@
alignment :: 'CppEnum'
alignment =
  'makeEnum' ('ident' \"Alignment\") Nothing
  [ (0, [\"left\", \"align\"])
  , (1, [\"center\", \"align\"])
  , (2, [\"right\", \"align\"])
  ]
@

the following data type will be generated:

@
data Alignment =
    Alignment_LeftAlign
  | Alignment_CenterAlign
  | Alignment_RightAlign
@

with instances for 'Bounded', 'Enum', 'Eq', 'Ord', and 'Show'.

-}
{- $generators-hs-module-structure-bitspaces

'Bitspace's, unlike enums, materialize in Haskell using a single data
constructor and bindings for values, rather than multiple data constructors.  A
bitspace declaration such as

@
formatFlags :: 'Bitspace'
formatFlags =
  'makeBitspace' ('toExtName' \"Format\") 'TInt'
  [ (1, [\"format\", \"letter\"])
  , (2, [\"format\", \"jpeg\"])
  , (4, [\"format\", \"c\"])
  ]
@

will generate the following:

@
newtype Format

instance 'Bits' Format
instance 'Bounded' Format
instance 'Eq' Format
instance 'Ord' Format
instance 'Show' Format

fromFormat :: Format -> 'CInt'

class IsFormat a where
  toFormat :: a -> Format

instance IsFormat 'CInt'

format_FormatLetter :: Format
format_FormatJpeg :: Format
format_FormatC :: Format
@

-}
{- $generators-hs-module-structure-functions

For a 'Function' export, a single Haskell function will be generated named after
the external name of the export.  The function will take the Haskell-side types
of its arguments, and return the Haskell-side type of its return type.  If the
function is 'Nonpure' then it will return a value in 'IO', otherwise it will
return a pure value using 'unsafePerformIO'.

For most 'Type's, the corresponding Haskell parameter type will be a concrete
type.  This differs for objects (and references and pointers to them), where
typeclass constraints are used to implement C++ parameter type contravariance.
See the section on Haskell object passing for more details.

-}
{- $generators-hs-module-structure-callbacks

Despite needing to be exported as with other 'Export' choices, 'Callback's do
not expose anything to the user.  Instead, they provide machinery for functions
to be able to use 'TCallback'.

-}
{- $generators-hs-module-structure-classes

'Class'es expose quite a few things to the user.  Take a simple class
definition such as:

@
compressor :: 'Class'

zipper :: 'Class'
zipper =
  'makeClass' ('ident' \"Zipper\") Nothing [compressor]
  [ 'mkCtor' \"new\" [] ]
  [ 'mkStaticMethod' \"canZip\" [] 'TBool'
  , 'mkConstMethod' \"hasZipped\" [] 'TVoid'
  , 'mkMethod' \"zip\" [] 'TVoid'
  ]
@

Let's focus on @zipper@.  Two data types will be generated that represent
const and non-const pointers to @Zipper@ objects:

@
newtype Zipper
newtype ZipperConst

zipper_null :: Zipper
@

These types will have instances of 'CppPtr'.  There will also be some
typeclasses generated, for types that represent @Zipper@ objects:

@
class ZipperValue a where
  withZipperPtr :: a -> (ZipperConst -> IO b) -> IO b

instance CompressorPtrConst a => ZipperValue a

class CompressorPtrConst a => ZipperPtrConst a where
  toZipperConst :: a -> ZipperConst

class (ZipperPtrConst a, CompressorPtr a) => ZipperPtr a where
  toZipper :: a -> Zipper

instance ZipperPtrConst ZipperConst
instance ZipperPtr Zipper
... instances required by superclasses ...
@

Ignoring the first typeclass and instance for a moment, the two @Ptr@
typeclasses represent const and non-const pointers respectively, and allow
upcasting pointer types.  The const typeclass has as superclasses the const
typeclasses for all of the C++ class's superclasses (or just 'CppPtr' if this
list is empty).  The non-const typeclass has as superclasses the non-const
typeclasses for all of the C++ class's superclasses, plus the current const
typeclass.  Instances will be generated for all of the appropriate typeclasses
for 'Zipper' and 'ZipperConst', all the way up to 'CppPtr'.

The @ZipperValue@ class represents general @Zipper@ values, of which pointers
are one type (hence the first @instance@ above).  Values of these types can be
converted to a temporary const pointer.  If @Zipper@ were to have a native
Haskell type (see 'classHaskellConversion'), then an additional instance would
be generated for that type.  This second instance in this case is overlapping,
and the above instance is overlappable.  These typeclasses allow for mixing
pointer, reference, and object types when calling C++ functions.

Finally, Haskell functions are generated for all of the class's constructors and
methods.  These work much the same as function exports, but non-static methods
take a @this@ object as the first argument.  No specialized deletion method is
needed; 'delete' is generic.

@
zipper_new :: 'IO' Zipper
zipper_canZip :: 'IO' 'Bool'
zipper_hasZipped :: ZipperValue this => this -> 'IO' 'Bool'
zipper_zip :: ZipperPtr this => this -> 'IO' 'Bool'
@

Downcasting pointers is not currently supported.  Beware that the obvious
approach of

@
compressorToZipper :: Compressor -> Zipper
compressorToZipper = Zipper . 'castPtr' . 'toPtr'
@

doesn't work in the presence of multiple inheritance, where casting requires
pointer arithmetic.

-}
{- $generators-hs-module-dependencies

While generated C++ modules get their objects from @#include@s of underlying
headers and only depend on each other in the case of callbacks, Haskell modules
depend on each other any time something in one references something in another
(somewhat mirroring the dependency graph of the binding definitions), so cycles
are much more common (for example, when a C++ interface uses a forward class
declaration to break an @#include@ cycle).  Fortunately, GHC supports dependency
cycles, so Hoppy automatically detects and breaks cycles with the use of
@.hs-boot@ files.  The boot files contain everything that could be used from
another generated module, for example class casting functions needed to coerce
pointers to the right type for a foreign call, or enum data declarations.  The
result of this cycle breaking is deterministic: for each non-trivial strongly
connected component in the module dependency graph, @.hs-boot@ files are
generated for all modules, and all @.hs@ files' dependencies within the SCC
import @.hs-boot@ files.

-}
{- $generators-hs-object-passing

All of the comments about argument passing for the C++ generator apply here.
The following types are used for passing arguments from Haskell to C++:

>  C++ type   | Pass over FFI | HsCSide  | HsHsSide
> ------------+---------------+----------+-----------------
>  Foo        | Foo const*    | FooConst | FooValue a => a
>  Foo const& | Foo const*    | FooConst | FooValue a => a
>  Foo&       | Foo*          | Foo      | FooPtr a => a
>  Foo const* | Foo const*    | FooConst | FooValue a => a
>  Foo*       | Foo*          | Foo      | FooPtr a => a

@FooPtr@ contains pointers to nonconst @Foo@ (and all subclasses).  @FooValue@
contains pointers to const and nonconst @Foo@ (and all subclasses), as well as
the convertible Haskell type, if there is one.  The rationale is that @FooValue@
is used where the callee will not modify the argument, so both a const pointer
to an existing object, and a fresh const pointer to a temporary on the case of
passing a @Foo@, are fine.  Because functions taking @Foo&@ and @Foo*@ may
modify their argument, we disallow passing a temporary converted from a Haskell
value implicitly; 'withCppObj' can be used for this.

For values returned from C++, and for arguments and return values in callbacks,
the 'HsCSide' column above is the exposed type; polymorphism as in the
'HsHsSide' column is not provided.

-}
