Nix expressions are provided to ease building Hoppy within
[Nixpkgs](https://nixos.org/nixpkgs), to demonstrate building sample programs,
and to run unit tests.  There is a `default.nix` for each package in the
repository, as well as a few other Nix files:

- `/pkgs.nix` provides a modified Nixpkgs with all of the Hoppy packages
  inserted.  You can use this as a model for your own `~/.nixpkgs/config.nix`.

- `/config.nix` may optionally be created to pass additional arguments to the
  `hoppy` package.  `pkgs.nix` will use this file if it exists.  See below for
  additional options this package accepts.

- `/tests/default.nix` is a `nix-build`able expression that builds and runs all
  of the Hoppy tests.

The `hoppy` package accepts two optional parameters.  `enableSplitObjs`, when
non-null, will override Nixpkgs's default behaivour for Cabal, and the boolean
`forceParallelBuilding` will force a parallel build for faster development, at
the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).

---

This file is part of Hoppy.

Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>

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
