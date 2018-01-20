-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

  -- ** A first binding
  -- $getting-started-a-first-binding

  -- ** Types
  -- $getting-started-types

  -- ** Wrapping up the string binding
  -- $getting-started-wrapping-up-the-string-binding

  -- ** Functions
  -- $getting-started-functions

  -- ** Objects
  -- $getting-started-objects

  -- *** Generated bindings
  -- $getting-started-objects-generated-bindings

  -- *** Passing and returning objects
  -- $getting-started-objects-passing-and-returning-objects

  -- *** Garbage collection
  -- $getting-started-objects-garbage-collection

  -- *** Conversions
  -- $getting-started-objects-conversions

  -- ** API versioning
  -- $getting-started-api-versioning

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

  -- *** Exceptions
  -- $generators-hs-exceptions
  ) where

import Data.Bits (Bits)
import Foreign.C (CInt, peekCString, withCString)
import Foreign.Hoppy.Generator.Language.Haskell
import Foreign.Hoppy.Generator.Main
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Version
import Foreign.Hoppy.Runtime
import Foreign.Ptr (FunPtr, Ptr)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )
import System.IO.Unsafe (unsafePerformIO)

{- $overview

Hoppy is a foreign function interface (FFI) generator for interfacing Haskell
with C++.  It lets developers specify C++ interfaces in pure Haskell, and
generates code to expose that functionality to Haskell.  Hoppy is made up of a
few different packages that provide interface definition data structures and
code generators, some runtime support for Haskell bindings, and interface
definitions for the C++ standard library.

Bindings using Hoppy have three Cabal packages:

- A Haskell generator program (in @\/myproject-generator@) that knows the
interface definition and generates code for the next two parts.

- A C++ library (in @\/myproject-cpp@) that gets compiled into a shared object containing
the C++ half of the bindings.

- A Haskell library (in @\/myproject@) that links against the C++ library and
exposes the bindings.

The path names are suggested subdirectories of a project, and are used in this
document, but are not required.  Only the latter two items need to be packaged
and distributed to users of the binding (plus Hoppy itself which is a dependency
of the generated bindings).

-}
{- $getting-started

This section provides a gentle introduction to working with Hoppy.

-}
{- $getting-started-project-setup

To set up a new Hoppy project, it's recommended to start with the project in the
@example\/@ directory as a base.  It is a minimal project that defines a C++
function to reverse a @std::string@, exposes that to Haskell via a library, and
provides a demo program that uses the library.  The @example\/install.sh@ script
simply compiles and installs the generator, C++, and Haskell packages in turn.

The generator package specifies the C++ interface to be exposed, using the
functions and data types described in the rest of this section.

The C++ package is a mostly empty, primarily containing a @Setup.hs@ file that
invokes Hoppy build hooks, and the C++ code we're binding to.  When building
this package, Hoppy generates some C++ code and then relies on a Makefile we
provide for linking it together with any code we provided (see
@example\/example-cpp\/cpp\/Makefile@).  If you are relying on a system library,
you can link to it in the Makefile.

The Haskell package is even more empty than the C++ one.  It contains a similar
@Setup.hs@ to invoke Hoppy.  Nothing else is included in the package's library,
although you are free to add your own Haskell modules.  The executable ties
everything together by calling the C++ code.  It reverses the characters of each
input line it sees.

To publish this project, one would upload all three packages to Hackage.  (But
make sure to rename it first!)

-}
{- $getting-started-a-first-binding

A complete C++ API is specified using Haskell data structures in
"Foreign.Hoppy.Generator.Spec".  At the top level is the 'Interface' type.  An
interface contains 'Module's which correspond to portions of functionality of
their interface; that is, collections of classes, functions, files, etc.
Functionality can be grouped arbitrarily into modules and doesn't have to follow
the structure of existing C++ files.  Modules contain 'Export's which refer to
concrete bound entities ('Function's, 'Class'es, etc.).

For starters, we will look at a single class.  Let's write a binding for
@std::string@.  An initial version could start as follows.

@
import "Foreign.Hoppy.Generator.Spec"

c_string :: 'Class'
c_string =
  'addReqIncludes' ['includeStd' \"string\"] $
  'makeClass' ('ident1' \"std\" \"string\") (Just $ 'toExtName' \"StdString\")
  []
  [ 'mkCtor' \"new\" []
  , 'mkConstMethod' \"at\" ['intT'] 'charT'
  , 'mkConstMethod' \"string\" [] 'sizeT'
  ]
@

There is quite a bit to look at here, so let's work through it.

First, everything that can be exported has two names besides the name used for
the Haskell binding (@c_string@ above).  'Identifier's are used to specify the
qualified C++ names of exports, including namespaces and template arguments.
For this example, our identifier is @std::string@, which we specify with the
'ident1' call above.  The number indicates the number of leading namespace
components.  'ident' can be used for top-level entities.

Exported entities also each have an /external name/ that uniquely identifies it
within an interface.  This name can be different from the name of the C++ entity
the export is referring to.  An external name is munged by the code generators
and must be a valid identifier in all languages a set of bindings will use, so
it is restricted to characters in the range @[a-zA-Z0-9_]@, and must start with
an alphabetic character.  Character case in external names will be preserved as
much as possible in generated code, although case conversions are sometimes
necessary (e.g. Haskell requiring identifiers to begin with upper or lower case
characters).  In the example, the 'toExtName' call specifies an explicit
external name for the class.  @Nothing@ may be provided to automatically derive
an external name from the given identifier.  The derived name is based on the
last component of the identifier, which in this case is just @string@.
Converting this to a Haskell type name gives @String@, which collides with the
built-in string type, so we give an explicit external name instead.

The third argument to 'makeClass' is a list of superclasses.  @std::string@ does
not derive from any classes so we leave this empty in the example.  When
specifying interfaces in Hoppy, only publicly accessible components need to (and
in fact, can) be referenced by Hoppy: public base classes, public methods and
variables, but never protected or private entities.

The final argument to 'makeClass' is a list of entities within the class.  Here
we can specify constructors, methods, and variables that the class exposes, via
the 'ClassEntity' type.  There are a few sets of methods for building class
entities:

- Basic forms: 'makeCtor', 'makeMethod', and 'makeClassVariable' are the core
functions for building class entities.  These are fully general, and take
parameters both for the C++ and external names, as well as staticness,
constness, etc.  There is also 'makeFnMethod' for defining a method that is
actually backed by a C++ function, not a method of the class; this can be used
when manual wrapping of a method is required, to wrap a method with a function
but make it look like a method.

- Convenience forms: 'mkCtor', 'mkMethod', 'mkConstMethod', 'mkStaticMethod',
'mkProp', 'mkBoolIsProp', 'mkBoolHasProp', 'mkStaticProp', and 'mkClassVariable'
only take the C++ name, and derive the external name from it, as well as
assuming other parameters (staticness, etc.).  These are what you typically use,
unless you need overloading, in which case use the overloading forms below.

- Overloading forms: 'mkMethod'', 'mkConstMethod'', and 'mkStaticMethod'' are
convenience functions for overloaded methods.  Overloading is handled by
defining multiple exports all pointing to a single C++ entity.  These in turn
become separate Haskell functions.  Because external names must be unique
though, a different external name must be provided for each overloaded form;
this is the second argument to these functions.

- Unwrapped forms: Underscore forms for all of the above are provided as well
(e.g. 'mkMethod_' and 'mkMethod'_') that return the actual object they create
('Method', 'Ctor', 'ClassVariable') instead of wrapping the result in a
'ClassEntity' as is usually desired.

Generated C++ bindings for exported entities usually need @#include@s in order
to access those entities.  This is done with 'Include' and 'Reqs' types.  When
defining bindings, all exportable types have an instance of the 'HasReqs'
typeclass, and 'addReqIncludes' can be used to add includes.  'includeStd'
produces @#include \<...>@ statements, and 'includeLocal' produces @#include
\"...\"@.

This use of 'addReqIncludes' also indicates a common pattern for writing class
bindings.  After constructing a 'Class' with 'makeClass', there are a number of
functions that modify the class definition in various ways.  These functions'
types always end in @... -> 'Class' -> 'Class'@, so that they can be chained
easily.  Among others, these functions include:

- 'addReqIncludes' to add needed C++ @#include@ statements.
- 'classAddEntities' to add additional entities to a class.
- 'classAddFeatures' to add common functionality to a class.
- 'classMakeException' to add exception support for a class.
- 'classSetConversion' to configure an implicit conversion.

The main point to using all of these functions is to chain them on the result of
'makeClass', but to bind the Haskell binding to final resulting value.  For
instance, if we have the following class:

@
c_NotFoundException :: 'Class'
c_NotFoundException =
  'addReqIncludes' ['includeStd' \"exceptions.hpp\"] $
  'classMakeException' $
  'makeClass' ('ident' \"NotFoundException\") Nothing []
  [ 'mkCtor' \"newCopy\" ['objT' c_NotFoundException]
  ]
@

Then, 'addReqIncludes' and 'classMakeException' modify the class object, and the
constructor definition makes use of the resulting object.  This works as
intended.  In some cases the order of modifiers is important -- for example,
marking a class as an exception class requires that there be a copy constructor
defined beforehand -- but usually order of modification does not matter.

Another point of note is the @c_@ prefix used on these two classes.  A suggested
naming convention for entities is:

- @v_@ for variables ('Variable').
- @f_@ for functions ('Function').
- @c_@ for classes ('Class').
- @e_@ for enums ('CppEnum').
- @bs_@ for bitspaces ('Bitspace').
- @cb_@ for callbacks ('Callback').
- @mod_@ for modules ('Module').

Hoppy follows this convention, but you are not required to in your own bindings.
It enables the use of proper casing on the actual entity name, and avoids
collision with existing Haskell names.

Given all this, we can improve our @std::string@ binding.  The @at()@ method
provides a non-const overload that returns a reference to a requested character.
Let's have two versions of @at()@, as well as expose the fact that @std::string@
is assignable, comparable, copyable, and equatable, with a second version:

@
c_string :: 'Class'
c_string =
  'addReqIncludes' ['includeStd' \"string\"] $
  'classAddFeatures' ['Assignable', 'Comparable', 'Foreign.Hoppy.Generator.Spec.ClassFeature.Copyable', 'Equatable'] $
  'makeClass' ('ident1' \"std\" \"string\") (Just $ 'toExtName' \"StdString\")
  []
  [ 'mkCtor' \"new\" []
  , 'mkConstMethod'' \"at\" \"at\" ['intT'] $ 'refT' 'charT'
  , 'mkConstMethod'' \"at\" \"get\" ['intT'] 'charT'
  , 'mkConstMethod' \"string\" [] 'sizeT'
  ]
@

-}
{- $getting-started-types

Let's take a break from @std::string@ for a moment and talk about how we
represent data types in Hoppy.

All C++ types are represented with the 'Type' data type, values of which are in
the "Foreign.Hoppy.Generator.Types" module.  This includes primitive numeric
types, object types, function types, pointers and references, @void@, the const
qualifier, etc.

A Hoppy 'Type' value has a corresponding C++ type, and also what we refer to as
a C type, and possibly a Haskell type.  The C++ type is, of course, whatever C++
type the 'Type' structure represents.  The Haskell type is the Haskell data type
that the C++ value gets converted to and from when using the bindings from
Haskell code.  The C type is the C data type that gets passed over the gateway
between C++ and Haskell, because these types are the common denominator between
C++ and Haskell.  When passing values back and forth between C++ and Haskell,
generally, primitive types are converted to equivalent types on both ends, and
pointer types in C++ are represented by corresponding pointer types in Haskell.
Other types have special rules.  Conversion code is generated on both sides of
the gateway to perform the necessary conversions.

For numbers, the Haskell FFI provides numeric types in "Foreign.C" for
interfacing with C directly.  Hoppy maps C++ numbers to these types, with the
exception of @bool@, @int@, @float@, and @double@, which map to their native
Haskell equivalents instead, for convenience ('Bool', 'Int', 'Float', 'Double').

The mapping between C++ and Haskell types is as follows.  Types that we haven't
covered are listed here for completeness.  Some of these aren't really types,
but are modifiers that are only useful in certain situations.

- 'voidT': C++ uses @void@.  The C type is unspecified.  Haskell uses @()@.

- 'boolT', 'intT', 'floatT', 'doubleT': C++ and C use @bool@, @int@, @float@,
@double@.  Haskell uses 'Bool', 'Int', 'Float', 'Double'.

- 'charT', 'ucharT', and other primitive numeric types: Identical C++ and C
types; Haskell uses built-in foreign data types.  See
"Foreign.Hoppy.Generator.Types" for more info.

- 'enumT': C++ uses the enum type.  C uses @int@.  Haskell uses a generated data
type for the enum.

- 'bitspaceT': C++ and C use a specified type, usually @int@.  Haskell uses a
generated data type for the bitspace.

- 'ptrT': C++ and C use the raw pointer type.  The Haskell type depends on the
pointed-to type.  If it's an object pointer, then Haskell uses the generated
handle data type for the class.  If it's a function pointer, then Haskell uses a
'FunPtr' of the Haskell function in @IO@ with C types.  Otherwise, Haskell uses
a 'Ptr' to the C type of the pointed type.

- 'refT': C++ uses the reference type.  Everything else is the same as 'ptrT'.

- 'fnT': Function types aren't useful raw, and need to be wrapped in 'ptrT' to
be useable in parameter and return types.

- 'callbackT': C++ uses the callback's generated functor class.  C uses an
internal implementation pointer.  Haskell uses a function in @IO@ of the Haskell
types of the parameters and return types.

- 'objT': For objects passed by value (not wrapped in 'ptrT' or 'refT'), C++
uses the object type (or a const reference), C uses a pointer to the object, and
Haskell uses a type configured for the class.

- 'objToHeapT': Copies objects to the heap.

- 'toGcT': Assigns objects to the garbage collector.

- 'constT': Wraps types in @const@.

Many of these types (enumerations, object types, functions, callbacks) are
discussed in later sections.

-}
{- $getting-started-wrapping-up-the-string-binding

Let's package up the @std::string@ binding to get a buildable example.  We
define a module to export the class, and an interface to collect our modules.

@
import "Foreign.Hoppy.Generator.Main"
import "Foreign.Hoppy.Generator.Spec"

main :: IO ()
main = 'defaultMain' interfaceResult

interfaceResult :: Either String 'Interface'
interfaceResult = do
  iface <- 'interface' \"example\" [mod_string]
  'interfaceAddHaskellModuleBase' [\"Example\"] iface

mod_string :: 'Module'
mod_string =
  'moduleModify'' ('makeModule' \"mystring\" \"mystring.hpp\" \"mystring.cpp\") $
  'moduleAddExports' ['ExportClass' c_string]
@

Each 'Module' produces separate C++ files to be compiled (a header and a source
file), and a separate Haskell module.  The Haskell module will be named
@Example.Mystring@, which is the concatenation of the Haskell module base path
defined on the interface, and the case-corrected name of the module.  A custom
module path can replace the default with 'moduleAddHaskellName'.

-}
{- $getting-started-functions

Let's say we have a C++ function to compute the hypotenuse of a triangle:

> double hypotenuse(double x, double y);

To declare a binding for this, we can write:

@
f_hypotenuse :: 'Function'
f_hypotenuse =
  'makeFn' \"hypotenuse\" Nothing 'Pure' ['doubleT', 'doubleT'] 'doubleT'
@

Like the first two arguments to 'makeClass', the first two arguments to 'makeFn'
are its C++ name, and an optional external name that will be derived from the
C++ name if absent.

The third argument indicates whether or not the function is pure.  'Nonpure'
generates a function in @IO@, and 'Pure' generates a pure function (using
'unsafePerformIO' internally).  Most functions should be marked as nonpure,
either because they have side effects or because you want to control the order
of their execution with respect to other functions with side effects, but in
this case (assuming the implementation of @hypontenuse()@ just performs the
calculation) we can mark it as pure.

Finally, there are the parameter type list and return type.

In Haskell, this will generate the following function, no surprises here:

@
hypotenuse :: 'Double' -> 'Double' -> 'Double'
@

-}
{- $getting-started-objects

There is a lot more to using classes than there is with functions, so we'll
spend the next several sections discussing how objects work.

-}
{- $getting-started-objects-generated-bindings

Now that we've seen what is generated for a function, let's see what is
generated for our @std::string@ binding.  Here is the definition again:

@
c_string :: 'Class'
c_string =
  'addReqIncludes' ['includeStd' \"string\"] $
  'classAddFeatures' ['Assignable', 'Comparable', 'Foreign.Hoppy.Generator.Spec.ClassFeature.Copyable', 'Equatable'] $
  'makeClass' ('ident1' \"std\" \"string\") (Just $ 'toExtName' \"StdString\")
  []
  [ 'mkCtor' \"new\" []
  , 'mkConstMethod'' \"at\" \"at\" ['intT'] $ 'refT' 'charT'
  , 'mkConstMethod'' \"at\" \"get\" ['intT'] 'charT'
  , 'mkConstMethod' \"string\" [] 'sizeT'
  ]
@

The first thing that is generated are data types called __handles__ that
represent pointers to instances.  For each class, there are two handle types, a
const and a nonconst version.  These are distinct types so that we can enforce
const-safety.  Functions for casting constness are created as well.

@
data StdString
data StdStringConst

castStdStringToConst :: StdString -> StdStringConst
castStdStringToNonconst :: StdStringConst -> StdString

instance 'Eq' StdString
instance 'Eq' StdStringConst
instance 'Ord' StdString
instance 'Ord' StdStringConst
instance 'Show' StdString
instance 'Show' StdStringConst
@

These instances operate on the underlying pointer.  There is also a
"Foreign.Hoppy.Runtime" module in the @hoppy-runtime@ package that holds various
types and code needed at runtime, and generated bindings make extensive use of
this package.  There are a number of class-related typeclasses there that also
contain these types.

@
instance 'CppPtr' StdString
instance 'CppPtr' StdStringConst
instance 'Deletable' StdString
instance 'Deletable' StdStringConst
instance 'Foreign.Hoppy.Runtime.Copyable' StdString StdString
instance 'Foreign.Hoppy.Runtime.Copyable' StdStringConst StdString
@

'CppPtr' is a typeclass for all handle types.  Unless a class is marked as
having a private destructor ('classSetDtorPrivate'), you will be able to delete
it with 'delete'; deletability also enables garbage collection, discussed later.
Finally, if a class has a copy constructor defined for it (either manually with
'mkCtor' or via the 'Foreign.Hoppy.Generator.Spec.ClassFeature.Copyable' class
feature), then it gets instances of 'Foreign.Hoppy.Runtime.Copyable'.

Next, a set of typeclasses are generated to hold values representing the class,
strings in this case.

@
class StdStringValue a

class 'CppPtr' this => StdStringConstPtr this where
  toStdStringConst :: this -> StdStringConst

class StdStringConstPtr this => StdStringPtr this where
  toStdString :: this -> StdString

instance {OVERLAPPABLE} StdStringConstPtr a => StdStringValue a  -- (sic)

instance StdStringConstPtr StdStringConst

instance StdStringConstPtr StdString
instance StdStringPtr StdString

instance StdStringValue a => 'Assignable' StdString a
@

The three typeclasses contain string-like types.  @StdStringValue@ is the most
general, and contains types that can be represented as a @std::string@.  For now
this includes the handle types; we'll see how to make good use of this typeclass
in a bit.

The other two typeclasses contain @std::string@ handles.  @StdStringConstPtr@
contains all handles that via C++ implicit casting could be converted to a
@StdStringConst@, and likewise for @StdStringPtr@ and @StdString@.  This gives
the instances above.  The typeclasses' methods can be used to upcast, and
optionally add const.

In a hierarchy with derived classes, this would be more complicated.  For an
arbitrary class, the const typeclass has as superclasses the const typeclasses
for all of the C++ class's superclasses (or just 'CppPtr' if this list is
empty).  The non-const typeclass has as superclasses the non-const typeclasses
for all of the C++ class's superclasses, plus the current const typeclass.
Instances will be generated for all handles as appropriate.  This formalises
const handles as parallel to nonconst handles in the hierarchy, but also above.

The overlappable instance just says that all @std::string@ handles, const or
nonconst, are @std::string@ values.  Because we defined an assignment operator
(@operator=@), we get an 'Assignable' instance, so that if we have a
@StdStringValue@ value, we can assign it to a @StdString@ handle by calling that
operator.

We can also attempt downcasting using @dynamic_cast@.  If @std::string@ had
superclasses, their handles would have instances of the following typeclasses:

@
class StdStringSuper a where
  downToStdString :: a -> StdString

class StdStringSuperConst a where
  downToStdStringConst :: a -> StdStringConst
@

Like @dynamic_cast@, these methods will return 'nullptr' if the given object is
not of an appropriate type.

-}
{- $getting-started-objects-passing-and-returning-objects

So how do we work with functions that expect objects?  Suppose we have the
following function:

> void reverse(std::string&);

Then if we define:

@
f_reverse :: 'Function'
f_reverse = 'makeFn' \"reverse\" Nothing 'Nonpure' ['refT' $ 'objT' c_string] voidT
@

We'll get the following binding:

@
reverse :: StdStringPtr this => this -> IO ()
@

Unlike primitive types, parameters for object types use typeclass constraints to
accept a range of types, so that subclasses' handles can also be passed.

There are five different object types that can be used in parameter and return
types:

1. @('refT' $ 'constT' $ 'objT' c_string)@ is @const std::string&@.
2. @('refT' $ 'objT' c_string)@ is @std::string&@.
3. @('ptrT' $ 'constT' $ 'objT' c_string)@ is @const std::string*@.
4. @('ptrT' $ 'objT' c_string)@ is @std::string*@.
5. @('objT' c_string)@ is @std::string@.

When used as a parameter type, cases 1 and 3 generate a Haskell parameter type
of @StdStringConstPtr a => a@, and cases 2 and 4 generate a Haskell parameter
type of @StdStringPtr a => a@.  There is no distinction between references and
pointers in Haskell, but if the C++ function expects a mutable object, then so
does the Haskell binding.  Case 5 generates a Haskell parameter type of
@StdStringValue a => a@.

You should use the type that the actual function expects, with one exception.
When the type of a parameter of a C++ function is @const C&@ (case 1), it is
recommended to just declare it as @C@ (case 5), since this is shorter and is
fully equivalent when passing a handle (but is also open to the object having a
conversion added in the future).  This exception does not apply to function
return types.

When returning values from a function, cases 1 and 3 return a @StdStringConst@,
cases 2 and 4 return a @StdString@, and what case 5 returns depends on the
class's defined conversion (discussed later).

-}
{- $getting-started-objects-garbage-collection

By default, object lifetimes are managed manually.  They are created with
constructors and eventually destroyed with 'delete' (or explicitly scoped via
'withScopedPtr').  Alternatively, ownership of objects can be passed to the
Haskell garbage collector, to be deleted when no references are left from
Haskell memory to the object.

This is tracked internally by handles.  Handles can either be unmanaged (as they
are initially) or managed (by the collector).  For simplicity, this is not
reflected in a handle's type.  A managed handle can be created from an unmanaged
handle by calling 'toGc'.  This assigns the object to be tracked by the
collector, and 'delete' will be called on the object once no more handles are
left pointing to it.  'toGc' returns a __new__ handle, and existing unmanaged
handles for the object should no longer be used, since they will be dangling
pointers once the object is destroyed.  There are some points of caution around
using this function that are worth knowing about; see the function documentation
for more info.

If you want to pass an object to the collector immediately upon creation, chain
its constructor call with @('toGc' '=<<')@.  This is not done by default because
we don't support revoking the collector's watch over an object, and there are
times when you want to work with manually managed objects.

'toGcT' may be used when defining a function to make an object being passed into
Haskell be managed by the garbage collector explicitly.  But rather than using
'toGcT' with value objects, it's better to use 'classSetConversionToGc'.  There
is also a lesser-used 'objToHeapT' for copying a temporary onto the heap for
Haskell code to manage without giving it to the garbage collector (and a
corresponding 'classSetConversionToHeap').

-}
{- $getting-started-objects-conversions

Object pointer and reference types (@'refT' . 'objT'@, @'ptrT' . 'constT'
. 'objT'@, etc.) use handles in Haskell to refer to objects living in C++
memory.  By-value object types (not pointers or references, just the by-value
object types, 'objT' directly) are treated differently.  When an object is taken
or returned by value, this typically indicates a lightweight or short-lived,
easily copied object, and Hoppy provides a few different behaviours to choose
from to handle these.  For instance, you can choose to use objects returned by
value via garbage-collected handles, or you can define automatic conversions to
and from a Haskell type.

This is all controlled by the 'ClassConversion' object that lives on each
'Class'.  Within, 'ClassHaskellConversion' has three fields that control
by-value behaviour:

- An optional Haskell data type is specified with 'classHaskellConversionType'.

- Optional logic for converting an object from a handle into an object of the
specified Haskell type is specified with 'classHaskellConversionFromCppFn'.

- Optional logic for converting a value of the specified Haskell type into a
handle is specified with 'classHaskellConversionToCppFn'.

The Haskell type here is the same as the Haskell type for 'objT' mentioned
earlier, in the section on types.  With the other two fields, we introduce the
concept of __class convertibility__.  A class can be convertible in zero, one,
or both directions to and from C++, depending on which of the latter two fields
above are specified.  Both of the conversion logic fields, if present, require
that a Haskell type be specified as well.

All of these fields include 'Generator' in their types.  This is a Haskell code
generation monad that supports line-based output and also manages module imports
and exports.  Refer to 'ClassHaskellConversion' and
"Foreign.Hoppy.Generator.Language.Haskell" for more detail on writing custom
conversions.

For ease of use with @std::string@, we want strings in C++ to convert to strings
in Haskell and vice versa, and we'll let the garbage collector handle the
Haskell strings.  We can do this by specifying the Haskell type as 'String', and
writing conversions:

@
c_string :: 'Class'
c_string =
  'addReqIncludes' ['includeStd' \"string\"] $
  'classAddFeatures' ['Assignable', 'Comparable', 'Foreign.Hoppy.Generator.Spec.ClassFeature.Copyable', 'Equatable'] $
  'classSetHaskellConversion'
    'ClassHaskellConversion'
    { 'classHaskellConversionType' = Just $ do
        'addImports' $ 'hsWholeModuleImport' \"Prelude\"
        'return' $ 'HsTyCon' $ 'UnQual' $ 'HsIdent' \"String\"
    , 'classHaskellConversionToCppFn' = Just $ do
        'addImports' $ 'mconcat' ['hsWholeModuleImport' \"Prelude\", 'hsWholeModuleImport' \"Foreign.C\"]
        'sayLn' \"'flip' 'withCString' stdString_newFromCString\"
    , 'classHaskellConversionFromCppFn' = Just $ do
        'addImports' $ 'mconcat' ['hsImport1' \"Control.Monad\" \"(<=<)\", 'hsWholeModuleImport' \"Foreign.C\"]
        'sayLn' \"'peekCString' <=< stdString_c_str\"
    } $
  'makeClass' ('ident1' \"std\" \"string\") (Just $ 'toExtName' \"StdString\")
  []
  [ 'mkCtor' \"new\" []
  , 'mkCtor' \"newFromCString\" ['ptrT' $ 'constT' 'charT']
  , 'mkConstMethod'' \"at\" \"at\" ['intT'] $ 'refT' 'charT'
  , 'mkConstMethod'' \"at\" \"get\" ['intT'] 'charT'
  , 'mkConstMethod' \"c_str\" [] $ 'ptrT' $ 'constT' 'charT'
  , 'mkConstMethod' \"string\" [] 'sizeT'
  ]
@

First we add @newFromCString@ and @c_str@ which we need to write the conversions.
Whenever writing Haskell generator code, you need to import whatever types you
want to use, in each of these individual actions, because they are used in
different places during generation.  The Haskell code that Hoppy writes only
uses qualified imports, under aliases that begin with @Hoppy@ (with the
exception of some standard infix operators which it imports unqualified).  It
does this to avoid conflicts between generated names and built-in ones, so you
could use an external name of @String@ if you wanted to.  You are welcome to
import modules wholesale, as the example here does.

@stdString_newFromCString@ and @stdString_c_str@ used in the conversion code are
the generated methods.  Conversion code is produced in the same Haskell module
as the rest of the Haskell code for a class, so the class's methods are always
present.  The to-C++ conversion takes a Haskell @String@, converts it to a
temporary C string, and creates and returns a @std::string@ from that.
Conversely, the from-C++ conversion takes a const @std::string@, grabs its C
string, and creates a Haskell string from it.

So how does this let us use strings more easily?  If we look back to what was
generated for our string class earlier, we had a @StdStringValue@ typeclass that
contained general @std::string@-like values.  When we define a conversion to
C++, then the Haskell @String@ also becomes an instance of this typeclass.  So
whenever a C++ function takes an argument of @std::string@, @const
std::string&@, or @const std::string*@, we can now pass a @String@ and have it
work automatically.  When a C++ function returns a @std::string@, it will now
automatically be converted to a @String@.  For example, given a function:

> std::string reverse(const std::string&);

And a method binding:

@
'makeFn' "reverse" Nothing 'Nonpure' ['objT' c_string] $ 'objT' c_string
@

We get the following Haskell function and instance:

@
reverse :: StdStringValue a => a -> IO 'String'

instance {OVERLAPPING} StdStringValue 'String'  -- (sic)
@

Note that when /returning/ an 'objT' from a function, there is no choice whether
the Haskell type or a handle is returned; the conversion is always performed.

If we just wanted to use the @StdString@ handle as the Haskell type for the
class, but have objects returned to Haskell be garbage collected, then in our
class definition we could use 'classSetConversionToGc' instead of
'classSetHaskellConversion'.  This would change @reverse@ to have the following
signature, and no @StdStringValue@ instance would be generated for 'String':

@
reverse :: StdStringValue a => a -> IO StdString
@

-}
{- $getting-started-api-versioning

Hoppy provides API versioning support in the "Foreign.Hoppy.Generator.Version"
module.  This is mainly done with the 'collect', 'just', and 'test' functions,
which are a simple wrapper around collecting a list of optional values:

@
type 'Filtered' = Maybe

'collect' :: ['Filtered' a] -> [a]
'none' :: 'Filtered' a
'just' :: a -> 'Filtered' a
'test' :: Bool -> a -> 'Filtered' a
@

These can be used anywhere a list is provided to Hoppy to filter based on some
criteria.  For example, the @std::pair@ binding in @hoppy-std@ defines a @swap@
method conditionally based on the version of the C++ standard being used:

@
c_pair :: 'Class'
c_pair =
  ... $
  'makeClass' ... $
  'collect'
  [ ...
  , 'test' ('activeCppVersion' >= 'Cpp2011') $ 'mkMethod' \"swap\" ['refT' $ 'objT' c_pair] 'voidT'
  ]
@

It is up to you to decide how to pass to your feature flags into your generator
(whether by environment variables, Cabal flags, etc.).  If you use environment
variables, you will need to use 'unsafePerformIO' to access them, since binding
definitions don't have access to @IO@.  For an example, see the implementation
of 'activeCppVersion'.

-}
{- $generators

This section describes the behaviour of the code generators, and documents what
they output for each type of export.

The code generators live at @Foreign.Hoppy.Generator.Language.\<language>@.  The
top-level module for a language is internal to Hoppy and contains the bulk of
the generator.  @General@ submodules expose functionality that can control
generator behaviour.

-}
{- $generators-cpp

The C++ code generator generates C++ bindings that other languages' bindings
will link against.  This generator lives in
"Foreign.Hoppy.Generator.Language.Cpp", with internal parts in
"Foreign.Hoppy.Generator.Language.Cpp.Internal".

-}
{- $generators-cpp-module-structure

Generated modules consist of a source and a header file.  The source file
contains all of the bindings for foreign languages to make use of.  The header
file contains things that may be depended on from other generated modules.
Currently this consists only of generated callback classes.

Cycles between generated C++ modules are not supported.  This can currently only
happen because of @#include@ cycles involving callbacks, since callbacks are the
only 'Export's that can be referenced by other generated C++ code.  Also, C++
callbacks that handle exceptions depend on the interface's exception support
module (see 'interfaceExceptionSupportModule').

-}
{- $generators-cpp-object-passing

@
'ptrT' :: 'Type' -> 'Type'
'refT' :: 'Type' -> 'Type'
'objT' :: 'Class' -> 'Type'
'constT' :: 'Type' -> 'Type'
@

We consider all of the following cases as passing an object, both into and out
of C++, and independently, as an argument and as a return value:

1. @'objT' _@
2. @'refT' ('constT' ('objT' _))@
3. @'refT' ('objT' _)@
4. @'ptrT' ('constT' ('objT' _))@
5. @'ptrT' ('objT' _)@

The first is equivalent to @'constT' ('objT' _)@.  When passing an argument from
a foreign language to C++, the first two are equivalent, and it's recommended to
use the first, shorter form (@T@ and @const T&@ are functionally equivalent in
C++, and are the same as far as what values foreign bindings will accept).

When passing any of the above types as an argument in either direction, an
object is passed between C++ and a foreign language via a pointer.  Cases 1, 2,
and 4 are passed as const pointers.  For a foreign language passing a @'objT' _@
to C++, this means converting a foreign value to a temporary C++ object.
Passing a @'objT' _@ argument into or out of C++, the caller always owns the
object.

When returning an object, again, pointers are always what is passed across the
language boundary in either direction.  Returning a @'objT' _@ transfers
ownership: a C++ function returning a @'objT' _@ will copy the object to the
heap, and return a pointer to the object which the caller owns; a callback
returning a @'objT' _@ will internally create a C++ object from a foreign value,
and hand that object off to the C++ side (which will return it and free the
temporary).

Object lifetimes can be managed by a foreign language's garbage collector.
'toGcT' is a special type that is only allowed in certain forms, and only when
passing a value from C++ to a foreign language (i.e. returning from a C++
function, or C++ invoking a foreign callback), to put the object under the
collector's management.  Only object types are allowed:

1. @'toGcT' ('objT' cls)@
2. @'toGcT' ('refT' ('constT' ('objT' cls)))@
3. @'toGcT' ('refT' ('objT' cls))@
4. @'toGcT' ('ptrT' ('constT' ('objT' cls)))@
5. @'toGcT' ('ptrT' ('objT' cls))@

Cases 2-5 are straightforward: the existing object is given to the collector.
Case 1 without the 'toGcT' would cause the object to be converted, but instead
here the (temporary) object gets copied to the heap, and a managed pointer to
the heap object is returned.  Case 1 is useful when you want to pass a handle
that has a non-trivial C++ representation (so you don't define a conversion for
it), but it's still a temporary that you don't want users to have to delete
manually.

Objects are always managed manually unless given to a garbage collector.  In
particular, constructors always return unmanaged pointers.  When a managed
pointer is passed into C++, that it is managed is lost in the FFI conversion,
and if this pointer is then passed back into the foreign language, it will
arrive in an unmanaged state (although the object is still managed, and it
should not be assigned to the collector a second time).

-}
{- $generators-cpp-callbacks

> data Callback = Callback ExtName [Type] Type ...  -- Parameter and return types.
>
> callbackT :: Callback -> Type

We want to call some foreign code from C++.  There are two choices for doing so,
described below.  Declaring a callback provides support for both types of
invocation.

__Function pointer:__ Function pointers are expressed with a @'ptrT' ('fnT'
...)@ type.  Foreign runtimes' FFIs can provide a means for creating raw
function pointers directly (Haskell's does with 'FunPtr').  Hoppy provides an
optional layer that performs the necessary type conversions, but only the
foreign half of the conversions, so only C types can be used within function
pointer types (this is a limitation of speaking over a C FFI; an error is
signaled when trying to use a type that requires C\<->C++ conversion).  The
other downside of using function pointers is that C++ provides no lifetime
tracking, and because in general foreign code can't know how long some C++ code
is going to hold a function pointer, it's necessary to manage the lifetime of
the pointer manually.

__C++ functor:__ This is the preferred method for calling into foreign code.
This type is expressed with 'callbackT'.  It wraps the function pointer support
above in C++ functors that add automatic lifetime tracking.

Internally, we create a class G that takes a foreign function pointer and
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
with internal parts in "Foreign.Hoppy.Generator.Language.Haskell.Internal".

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
  'makeBitspace' ('toExtName' \"Format\") 'intT'
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

Declared callbacks provide support for callback types ('callbackT') as well as
function pointers (@'ptrT' ('fnT' ...)@) in Haskell.

Callback types manifest directly as Haskell function types in @IO@.  Function
pointers manifest as 'FunPtr's around Haskell function types in @IO@.

No runtime support is exposed to the user for working with internal Haskell
callback types (some machinery is generated however).  For function pointer
types, a function @callbackName_newFunPtr@ is exposed from the callback's module
that makes it easy to wrap anonymous functions in 'FunPtr's that perform the
Haskell side of conversions, with code like the following:

> -- Generator bindings
>
> cb_intCallback = makeCallback "IntCallback" [intT] intT
>
> f_funPtrTest = makeFn "funPtrTest" Nothing Nonpure [ptrT $ fnT [intT] intT] intT
>
> f_callbackTest = makeFn "callbackTest" Nothing Nonpure [callbackT cb_intCallback] intT

> -- Test program
>
> import Foreign.C (CInt)
> import Foreign.Hoppy.Runtime (withScopedFunPtr)
>
> -- Generated things:
> intCallback_newFunPtr :: (Int -> IO Int) -> IO (FunPtr (CInt -> IO CInt))
> funPtrTest :: FunPtr (CInt -> IO CInt) -> Int
> callbackTest :: (Int -> IO Int) -> Int
>
> -- Driver code:
> callFunPtrTest = withScopedFunPtr (intCallback_newFunPtr $ return . (* 2)) funPtrTest
> callCallbackTest = callbackTest $ return . (* 2)

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
  [ 'mkStaticMethod' \"canZip\" [] 'boolT'
  , 'mkConstMethod' \"hasZipped\" [] 'voidT'
  , 'mkMethod' \"zip\" [] 'voidT'
  ]
@

Let's focus on @zipper@.  Two data types will be generated that represent
const and non-const pointers to @Zipper@ objects:

@
data Zipper
data ZipperConst
@

Internally, these types hold 'Ptr's, and they can be converted to 'Ptr's with
'toPtr' (though this conversion is lossy for pointers managed by the garbage
collector, see the section on object passing).

Several typeclass instances are generated for both types:

- 'Eq', 'Ord', and 'Show' compare and render based on the underlying pointer
address.

- 'CppPtr' and 'Deletable' instances provide object management.

- A single @'Decodable' ('Ptr' Zipper) Zipper@ instance is generated for
converting raw 'Ptr's into object handles.  This is the opposite operation of
'toPtr'.

- If the class -- @Zipper@ in this case -- has an @operator=@ method that takes
either a @'objT' zipper@ or a @'refT' ('constT' ('objT' zipper))@, then an
instance @ZipperValue a => 'Assignable' Zipper a@ is generated to allow
assigning of general zipper-like values to @Zipper@ objects; see below for an
explanation of @ZipperValue@.  This instance is for the non-const @Zipper@ only.

There will also be some typeclasses generated, for types that represent @Zipper@
objects:

@
class ZipperValue a where
  withZipperPtr :: a -> (ZipperConst -> IO b) -> IO b

instance CompressorConstPtr a => ZipperValue a

class CompressorConstPtr a => ZipperConstPtr a where
  toZipperConst :: a -> ZipperConst

class (ZipperConstPtr a, CompressorPtr a) => ZipperPtr a where
  toZipper :: a -> Zipper

instance ZipperConstPtr ZipperConst
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
for @Zipper@ and @ZipperConst@, all the way up to 'CppPtr'.

The @ZipperValue@ class represents general @Zipper@ values, of which pointers
are one type (hence the first @instance@ above).  Values of these types can be
converted to a temporary const pointer.  If @Zipper@ were to have a native
Haskell type (see 'classHaskellConversion'), then an additional instance would
be generated for that type.  This second instance in this case is overlapping,
and the above instance is overlappable.  These typeclasses allow for mixing
pointer, reference, and object types when calling C++ functions.

For downcasting, separate const and non-const typeclasses are generated with
instances for all direct and indirect superclasses of @Zipper@:

@
-- Enables downcasting from any non-const superclass of Zipper.
class ZipperSuper a where
  downToZipper :: a -> Zipper

-- Enables downcasting from any const superclass of Zipper.
class ZipperSuperConst a where
  downToZipperConst :: a -> ZipperConst

instance ZipperSuper Compressor
... instances for other non-const superclasses ...
instance ZipperSuperConst CompressorConst
... instances for other const superclasses ...
@

The downcast functions are wrappers around @dynamic_cast@, and will return a
null pointer if the argument is not a supertype of the target type.

Finally, Haskell functions are generated for all of the class's constructors and
methods.  These work much the same as function exports, but non-static methods
take a @this@ object as the first argument.  Const methods take a @ZipperValue@
on the assumption that it's safe to create a temporary C++ object from a Haskell
value if necessary to call a const method.  Non-const methods take a
@ZipperPtr@, since it's potentially a mistake to perform side-effects on a
temporary object that is thrown away immediately.

@
zipper_new :: 'IO' Zipper
zipper_canZip :: 'IO' 'Bool'
zipper_hasZipped :: ZipperValue this => this -> 'IO' 'Bool'
zipper_zip :: ZipperPtr this => this -> 'IO' 'Bool'
@

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

Object pointer types in Haskell hide whether they are managed (garbage
collected) or unmanaged pointers in their runtime representation.  The APIs that
bindings expose to Haskell users should generally not require them to be
concerned about object lifetimes, and also having separate data types for
managed pointers would balloon the size of bindings.  Unmanaged objects can be
converted to managed objects with 'toGc'; after calling this function, the value
it returns should always be used in place of any existing pointers.

-}
{- $generators-hs-exceptions

C++ exceptions can caught and thrown in Haskell.  C++ entities that deal with
exceptions need to be marked as such, for Hoppy to generate the support code for
them.  To work with exceptions at all, you need to pick one of your Hoppy
modules to contain some runtime support code, using
'interfaceSetExceptionSupportModule'.  C++ functions that throw need to be
marked with the specific exceptions that they throw, using 'handleExceptions'.
Callbacks that want to be able to throw need to be marked with
'callbackSetThrows', after which they are allowed to throw any exception classes
defined in the interface.  Exception handling in both directions can also be set
up at the module and interface levels using 'handleExceptions',
'interfaceSetCallbacksThrow', and 'moduleSetCallbacksThrow'.

Classes can be marked as being exception classes with 'classMakeException'.
Exception classes need to be copyable, so make sure to define a copy constructor
(use 'Copyable').

C++ exceptions in Haskell are handled with 'throwCpp' and 'catchCpp'.  While
they use Haskell exceptions under the hood, do not use 'throw' and 'catch' to
work with them; this may leak C++ objects.

Catching a wildcard (i.e. @catch (...)@) is supported, but no information is
available about the caught value.

Implementation-wise, an in-flight C++ exception in Haskell always owns the
object (which is on the heap).  An exception coming from C++ into Haskell (it's
a heap temporary) will be given to the garbage collector.  Hence, for ease of
use, caught exceptions should always be garbage-collected.  Also, when throwing
from Haskell, throwing will always take ownership of the object.  If 'throwCpp'
gets a non-GCed object, then it will be given to the garbage collector; and then
the exception will be thrown as a Haskell exception.  If the exception
propagates out to a callback and back into C++, then a temporary non-GCed copy
will be passed over the gateway, and rethrown as a value object on the C++ side.

In the above strategy, when throwing an exception from Haskell that propagates
to C++, it is wasteful to make the thrown object GCed, just to have to create a
non-GCed copy.  So when we throw from Haskell, we don't actually assign to the
garbage collector immediately (if it's not already); instead, we delay the
'toGc' call until 'catchCpp'.

-}
