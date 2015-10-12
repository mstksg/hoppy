# This is a modified Nixpkgs that contains all of the individual Cppop packages.

{ haskellOptions ? if builtins.pathExists ./config.nix
                   then import ./config.nix
                   else {}
}:

let

  cppopDir = ./.;

  haskellOverrides = haskellOptions: {
    overrides = self: super: {
      cppop = self.callPackage (cppopDir + /cppop) haskellOptions;

      cppop-example-templates-generator =
        self.callPackage (cppopDir + /examples/templates/generator) {};
      cppop-example-templates =
        self.callPackage (cppopDir + /examples/templates/hs) {};

      cppop-tests-basic-generator =
        self.callPackage (cppopDir + /tests/basic/generator) {};
      cppop-tests-basic =
        self.callPackage (cppopDir + /tests/basic/hs) {};

      cppop-tests-circular-generator =
        self.callPackage (cppopDir + /tests/circular/generator) {};
      cppop-tests-circular =
        self.callPackage (cppopDir + /tests/circular/hs) {};

      cppop-tests-stl-generator =
        self.callPackage (cppopDir + /tests/stl/generator) {};
      cppop-tests-stl =
        self.callPackage (cppopDir + /tests/stl/hs) {};
    };
  };

  packageOverrides = pkgs: rec {
    cppop-example-templates-lib = pkgs.callPackage (cppopDir + /examples/templates/lib) {
      inherit (haskellPackages) cppop-example-templates-generator;
    };

    cppop-tests-basic-lib = pkgs.callPackage (cppopDir + /tests/basic/lib) {
      inherit (haskellPackages) cppop cppop-tests-basic-generator;
    };

    cppop-tests-circular-lib = pkgs.callPackage (cppopDir + /tests/circular/lib) {
      inherit (haskellPackages) cppop cppop-tests-circular-generator;
    };

    cppop-tests-stl-lib = pkgs.callPackage (cppopDir + /tests/stl/lib) {
      inherit (haskellPackages) cppop cppop-tests-stl-generator;
    };

    haskellPackages = pkgs.haskellPackages.override (haskellOverrides haskellOptions);
  };

in import <nixpkgs> { config = { inherit packageOverrides; }; }
