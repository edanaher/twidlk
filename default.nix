{ pkgs ? import <nixpkgs> {}}:

pkgs.haskell.packages.ghc865.callPackage ./twidlk.nix { }
