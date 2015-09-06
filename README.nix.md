Nix expressions are provided to ease building Cppop within
[Nixpkgs](https://nixos.org/nixpkgs), and to demonstrate building sample
programs.  Cppop can be inserted into Nixpkgs with the following overrides,
after updating `cppopDir` as appropriate for your environment.

    packageOverrides = let cppopDir = /my/projects/cppop.git; in pkgs: rec {
      # Only to build the example:
      cppop-example-templates-lib = pkgs.callPackage (cppopDir + /examples/templates/lib) {
        inherit (haskellPackages) cppop-example-templates-generator;
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          cppop = self.callPackage cppopDir {};

          # Only to build the example:
          cppop-example-templates-generator = self.callPackage (cppopDir + /examples/templates/generator) {};
          cppop-example-templates = self.callPackage (cppopDir + /examples/templates/hs) {};
        };
      };

The `cppop` package accepts two optional parameters.  `enableSplitObjs`, when
non-null, will override Nixpkgs's default behaivour for Cabal, and the boolean
`forceParallelBuilding` will force a parallel build for faster development, at
the risk of nondeterministic results (see
[Nixpkgs bug 3220](https://github.com/NixOS/nixpkgs/issues/3220)).
