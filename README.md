# Hoppy - A C++ FFI generator for Haskell

Hoppy brings all the fun of pointers and manual memory management to Haskell,
making binding to C++ easy.

Homepage: http://khumba.net/projects/hoppy

Hoppy is free software under the GNU Affero General Public License, version 3,
the terms of which are in the `LICENSE` file.  I, Bryan Gardiner, reserve the
right (a) to release all AGPL parts of Hoppy under a future version of the AGPL
per section 14 of the AGPLv3, at my sole discretion, as well as the right (b) to
extend rights (a) and (b) to another entity.  By offering contributions to the
project, you accept these terms, and agree to license your contributions under
the project's current license(s).

Copyright 2015 Bryan Gardiner <bog@khumba.net>

## Installing

If you just want to install Hoppy to use it as a dependency of another project,
then all you need to do is build and install the Cabal package in `hoppy/`.

## Using

There is a user's guide written using Haddock at
`hoppy/src/Foreign/Hoppy/Documentation.hs`.  You can read it as is, or build it
with

    cd hoppy && cabal configure && cabal haddock

and browse the HTML files in `hoppy/dist/doc`.

## Developing

When submitting patches, please try to ensure that your changes compile cleanly
without warnings when `-W` is used, don't introduce new `hlint` lint, pass the
unit tests in the `tests/` directory, add new tests as appropriate, and follow
the style guide at:

http://khumba.net/projects/haskell-style
