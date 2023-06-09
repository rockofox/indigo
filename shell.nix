# { pkgs ? import <nixpkgs> }:
let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs;
      [ haskellPackages.cabal-install
        haskellPackages.ghcid
        pkgs.pkgsCross.wasi32.stdenv.cc
      ]);
}