with (import <nixpkgs> {});
with pkgs;
(haskellngPackages.callPackage ./. {
  archive = pkgs.libarchive;
}).env
