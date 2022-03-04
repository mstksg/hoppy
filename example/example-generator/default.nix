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

{ mkDerivation, base, hoppy-generator, hoppy-std, lib
}:
mkDerivation {
  pname = "hoppy-example-generator";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hoppy-generator hoppy-std ];
  executableHaskellDepends = [ base hoppy-generator ];
  doHaddock = false;
  license = lib.licenses.asl20;
}
