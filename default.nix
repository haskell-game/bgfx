{ mkDerivation, base, bytestring, clock, distributive, lens
, linear, mesa, sdl2, stdenv, x11
}:
mkDerivation {
  pname = "bgfx";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryPkgconfigDepends = [ mesa x11 ];
  executableHaskellDepends = [
    base bytestring clock distributive lens linear sdl2
  ];
  homepage = "https://github.com/haskell-game/bgfx";
  description = "Haskell bindings to bgfx";
  license = stdenv.lib.licenses.bsd3;
}
