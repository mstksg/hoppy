Nix expressions are provided to ease building Cppop within
[Nixpkgs](https://nixos.org/nixpkgs), to demonstrate building sample programs,
and to run unit tests.  There is a `default.nix` for each package in the
repository, as well as a few other Nix files:

- `/pkgs.nix` provides a modified Nixpkgs with all of the Cppop packages
  inserted.  You can use this as a model for your own `~/.nixpkgs/config.nix`.

- `/config.nix` may optionally be created to pass additional arguments to the
  `cppop` package.  `pkgs.nix` will use this file if it exists.  See below for
  additional options this package accepts.

- `/tests/default.nix` is a `nix-build`able expression that builds and runs all
  of the Cppop tests.

The `cppop` package accepts two optional parameters.  `enableSplitObjs`, when
non-null, will override Nixpkgs's default behaivour for Cabal, and the boolean
`forceParallelBuilding` will force a parallel build for faster development, at
the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).
