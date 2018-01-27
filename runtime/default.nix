# This file is part of Hoppy.
#
# Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

{ mkDerivation, base, stdenv, lib
, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "hoppy-runtime";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "http://khumba.net/projects/hoppy";
  description = "C++ FFI generator - Runtime support";
  license = stdenv.lib.licenses.asl20;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
