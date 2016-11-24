let
  pkgs = import ./nixpkgs.nix;
  hsPkgs = pkgs.haskell.packages;
in
  hsPkgs.ghc802.callPackage ./jni.nix {
    jdk = pkgs.openjdk8;
  }
