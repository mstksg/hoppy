# This file is part of Hoppy.
#
# Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This is the stock file generated with cabal2nix, with two exceptions:
#
# - A generated librarySystemDepends on 'example' (from "extra-libraries:
#   example" to depend on libexample.so) is incorrect to declare here;
#   libexample.so is provided through hoppy-example-cpp.  So it's simply
#   removed.
#
# - We have to turn on 'enableSharedExecutables'.  Executables built using Hoppy
#   require this.  In the Cabal file, there's no nicer way to achieve this than
#   doing "ghc-options: -dynamic", but this isn't detected by cabal2nix, so we
#   have to add it ourselves.

{ mkDerivation, base, Cabal, hoppy-example-cpp
, hoppy-example-generator, hoppy-runtime, HUnit, lib
}:
mkDerivation {
  pname = "hoppy-example";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [
    base Cabal hoppy-example-generator hoppy-runtime
  ];
  libraryHaskellDepends = [
    base hoppy-example-cpp hoppy-runtime
  ];
  # librarySystemDepends = [ example ];  (We don't want this.)
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hoppy-runtime HUnit ];
  license = lib.licenses.asl20;

  enableSharedExecutables = true;  # Must add this ourselves.
}
