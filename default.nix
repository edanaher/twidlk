{ pkgs ? import <nixpkgs> {}}:

pkgs.haskell.packages.ghc822.callPackage ./twidlk.nix { }
