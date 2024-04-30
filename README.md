# Hoppy - A C++ FFI generator for Haskell

Hoppy brings all the fun of pointers and manual memory management to Haskell,
making binding to C++ easy.

Homepage: https://khumba.net/projects/hoppy

User's Guide: https://hackage.haskell.org/package/hoppy-docs/docs/Foreign-Hoppy-Documentation-UsersGuide.html

Copyright 2015-2024 Bryan Gardiner <bog@khumba.net>

A range of successive copyright years may be written as XXXX-YYYY as an
abbreviation for listing all of the years from XXXX to YYYY inclusive,
individually.

The `LICENSE` file contains information about Hoppy's license.  Hoppy uses two
licenses, for different parts of the project.

## Using

Hoppy requires GHC 8.2 or newer.

If you just want to install Hoppy to use it as a dependency of another project,
then all you need to do is build and install the Cabal package in `runtime/`.

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

It can be built and installed with the `install.sh` script.

If you are using the Nix package manager, see `README.nix.md` for more
information about using Hoppy with it.

There is a user's guide written using Haddock at
`docs/src/Foreign/Hoppy/Documentation/UsersGuide.hs`.  You can read it as is, or
browse the HTML files in `docs/dist/doc` after building, or read it prerendered
on Hackage at the link at the top of this file.

## Developing

Patches are welcome!  Please enable the pre-commit hook at
`scripts/git-pre-commit` which checks lint and copyright/license issues:

    $ ln -s ../../scripts/git-pre-commit .git/hooks/pre-commit

Also please try to fix warnings that your changes introduce, check that the unit
tests in the `tests/` directory pass, and follow local style, or the
[style guide](https://git.sr.ht/~khumba/haskell-style/tree/master/item/haskell-style.md).
