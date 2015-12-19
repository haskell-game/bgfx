with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bgfx, stdenv }:
             mkDerivation {
               pname = "bgfx";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base ];
               extraLibraries = [ bgfx ];
               homepage = "https://github.com/haskell-game/bgfx";
               description = "Haskell bindings to bgfx";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
