{ nixpkgs ? import <nixpkgs> {}
, haskellPackages ? nixpkgs.haskellPackages
}:

haskellPackages.callPackage ./. {}
