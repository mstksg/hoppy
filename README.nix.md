Nix expressions are provided to ease building Hoppy within
[Nixpkgs](https://nixos.org/nixpkgs), to demonstrate building sample programs,
and to run unit tests.  There is a `default.nix` for each package in the
repository, as well as a collection of Nix expressions in the `nix/` directory.
The primary ones in this directory are described below.

For adding Hoppy to Nixpkgs in order to build it, and possibly your build own
packages against it, there are:

- `nix/overlay.nix` provides a Nixpkgs overlay that adds Hoppy packages to the
  Haskell packages set, for all compiler versions (both `haskell` and
  `haskellPackages` are modified).

- `nix/nixpkgs.nix` imports Nixpkgs and adds in the above overlay.

To ease building Hoppy itself, there are some `nix-build`-ready expressions:

- `nix/all-builds.nix` evaluates to a derivation that builds all Hoppy packages
  and symlinks the results into its output directory.  `all-builds-set.nix` can
  be used to build individual packages in this collection.

- `nix/tests.nix`, similarly to `all-builds.nix`, evaluates to a derivation that
  builds and runs all of the unit tests, plus the example binding, plus the
  Hoppy packages, symlinking the results into its output directory.  Individual
  packages can be built via `tests-set.nix`.

The above two files also have `-full.nix` versions that build against a few
different GHC versions, rather than just the current one.  This is controlled by
`haskells.nix`.

- `/config.nix` (*not* in `nix/`!) may optionally be created to pass additional
  arguments to the `hoppy` package.  The other Nix expressions will use this
  file if it exists.  See below for additional options this package accepts.

The Nix expressions for Hoppy packages accept one additional optional parameter.
The boolean `forceParallelBuilding` will force a parallel build for faster
development, at the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).

---

This file is part of Hoppy.

Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
