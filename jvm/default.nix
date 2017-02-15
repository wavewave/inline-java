let
  pkgs = import ./nixpkgs.nix;

  hsPkgs =
    pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc802 = pkgs.haskell.packages.ghc802.override {
          overrides = self: super: {
            jni =
              self.callPackage ../jni/jni.nix {
                jdk = pkgs.openjdk8;
              };
          };
        };
      };
    };
in
  hsPkgs.packages.ghc802.callPackage ./jvm.nix {
    jdk = pkgs.openjdk8;
  }
