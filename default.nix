with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bgfx, mesa, sdl2, stdenv }:
             mkDerivation {
               pname = "bgfx";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [ base sdl2 ];
               extraLibraries = [ bgfx mesa ];
               pkgconfigDepends = [ bgfx ];
               homepage = "https://github.com/haskell-game/bgfx";
               description = "Haskell bindings to bgfx";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
