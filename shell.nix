{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yi-config";

  buildInputs = [ haskellPackages.stack haskellPackages.cabal-install icu haskell.compiler.ghc7102 ];

  shellHook = ''
    export LD_LIBRARY_PATH="${icu}/lib:$LD_LIBRARY_PATH"
  '';
}