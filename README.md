# Hoppy - A C++ FFI generator for Haskell

Hoppy brings all the fun of pointers and manual memory management to Haskell,
making binding to C++ easy.

Homepage: http://khumba.net/projects/hoppy

Copyright 2015 Bryan Gardiner <bog@khumba.net>

The `LICENSE` file contains information about Hoppy's license.

## Dependencies

- GHC 7.8-7.10
- haskell-src 1.0-1.1 (for binding development only)
- mtl 2.1-2.3 (for binding development only)

If you just want to install Hoppy to use it as a dependency of another project,
then all you need to do is build and install the Cabal package in `runtime/`.

## Using

Hoppy consists of four Cabal packages:

- `generator/` holds the code generator library.  Developers specify interfaces
  in Haskell code, that then link against this library to produce a generator
  program that can create actual binding source code.  This package is only used
  while generating code, and isn't a dependency of generated bindings.

- `std/` provides interface definitions for the C++ standard library, including
  strings, containers, and iterators.

- `runtime/` is a runtime support library for Haskell bindings.  This is needed
  by generated Haskell code, so bindings created using Hoppy should list this as
  package as a dependency in their `.cabal` files.

- `docs/` contains documentation written using Haddock.  It depends on the
  previous packages for hyperlinks.

There is a user's guide written using Haddock at
`docs/src/Foreign/Hoppy/Documentation.hs`.  You can read it as is, or build it
with

    # Build the generator library documentation.
    cd generator && cabal configure && cabal haddock && cabal install
    # Build the standard library interface documentation.
    cd std && cabal configure && cabal haddock && cabal install
    # Build the runtime documentation.
    cd runtime && cabal configure && cabal haddock && cabal install
    # Build the documentation.
    cd docs && cabal configure && cabal haddock

and browse the HTML files in `docs/dist/doc`.

## Developing

When creating patches, please enable the pre-commit hook at
`scripts/git-pre-commit` which checks lint and copyright/license issues.  Also
try to ensure that your changes compile cleanly without warnings when `-W` is
used, pass the unit tests in the `tests/` directory, add new tests as
appropriate, and follow the style guide at:

http://khumba.net/projects/haskell-style
