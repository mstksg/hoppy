# Hoppy Examples

This directory contains example projects showing how to write bindings with
Hoppy.

`one-package-example`: This directory contains a bare-bones example showing how
to write bindings with a single Cabal package, making use of Cabal's
`cxx-sources` feature.

`three-package-example`: This directory contains an example that is split into
three separate Cabal packages: the generator, the C++ side of the bindings, and
the Haskell side of the bindings.  This is the tried-and-tested way and works
with ancient Cabal (pre-2.2) and custom C++ build systems, at the cost of more
packages and boilerplate.

----

Copyright 2015-2024 Bryan Gardiner <bog@khumba.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
