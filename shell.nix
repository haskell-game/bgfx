{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bgfx, mesa, stdenv, sdl2 }:
      mkDerivation {
        pname = "bgfx";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base sdl2 ];
        librarySystemDepends = [ bgfx mesa ];
        libraryPkgconfigDepends = [ bgfx ];
        homepage = "https://github.com/haskell-game/bgfx";
        description = "Haskell bindings to bgfx";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
