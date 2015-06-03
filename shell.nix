{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yi-config";

  buildInputs = [ haskellPackages.cabal-install icu haskell.compiler.ghc7101 ];

  shellHook = ''
    export LD_LIBRARY_PATH="${icu}/lib:$LD_LIBRARY_PATH"
  '';
}