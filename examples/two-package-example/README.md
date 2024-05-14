# Hoppy two-package example program

This is a small project that demonstrates use of Hoppy in interfacing with C++
code.  The `example` subdirectory is the wrapper package itself.
`example/cpp/utils.cpp` contains a function that reverses a string, and building
this package links to the C++ standard library (or whatever other libraries you
need to).  The binding definitions are split off into a separate package in the
`example-generator` directory, for maintainability.  This way, binding
definitions can be split into multiple Haskell modules freely.

Run `cabal v2-{build,run} hoppy-two-package-example` to build and run this
project.  (This can also be run from the root of the Hoppy repository, if you
have the full Hoppy repository available.)

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
