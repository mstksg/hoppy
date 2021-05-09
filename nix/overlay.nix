# This file is part of Hoppy.
#
# Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This file is a Nixpkgs overlay that adds in the (non-testing) Hoppy packages.

let

  haskellOptions =
    if builtins.pathExists ../config.nix
    then import ../config.nix
    else {};

  haskellOverrides = haskellLib: hself: hsuper:
    let buildStrictly = import ./build-strictly.nix haskellLib;
        jailbreak = drv: drv.override {
          mkDerivation = args: hsuper.mkDerivation ({ jailbreak = true; } // args);
        };
    in
    {
      # haskell-src complains when built with ghc865:
      #   Setup: Encountered missing dependencies:
      #   array >=0.5.4.0 && <0.6, base >=4.13.0.0 && <4.14
      # but ignoring dependency versions, it builds just fine, so we need to set
      # the jailbreak flag to ignore dependency versions for haskell-src.
      #
      # We also need guarantee we're running at least haskell-src-1.0.3.1.  The
      # NixOS 20.03 channel provides 1.0.3.0, which doesn't have the necessary
      # MonadFail changes for newer GHCs.  The replacement expression in this
      # case is derived from the one in hackage-packages.nix in Nixpkgs.
      haskell-src = jailbreak (
        let ver = (builtins.parseDrvName hsuper.haskell-src.name).version; in
        if builtins.compareVersions ver "1.0.3.1" > 0
        then hsuper.haskell-src
        else hsuper.callPackage
          ({ mkDerivation, array, base, happy, lib, pretty, syb }:
            mkDerivation {
              pname = "haskell-src";
              version = "1.0.3.1";
              sha256 = "0cjigvshk4b8wqdk0v0hz9ag1kyjjsmqsy4a1m3n28ac008cg746";
              revision = "2";
              editedCabalFile = "1qrhcyr0y7j8la3970pg80w3h3pprsp3nisgg1l41wfsr2m7smnf";
              libraryHaskellDepends = [ array base pretty syb ];
              libraryToolDepends = [ happy ];
              description = "Support for manipulating Haskell source code";
              license = lib.licenses.bsd3;
            }
          ) {}
      );
    } //
    builtins.mapAttrs (name: pkg: buildStrictly pkg) {
      hoppy-generator = hsuper.callPackage ../generator haskellOptions;
      hoppy-std = hsuper.callPackage ../std haskellOptions;
      hoppy-runtime = hsuper.callPackage ../runtime haskellOptions;
      hoppy-docs = hsuper.callPackage ../docs haskellOptions;
    };

in self: super: {
  haskell = super.haskell // {
    packageOverrides =
      super.lib.composeExtensions
        (super.haskell.packageOverrides or (_: _: {}))
        (haskellOverrides super.haskell.lib);
  };
}
