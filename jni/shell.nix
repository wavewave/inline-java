{ nixpkgs ? import ./nixpkgs.nix
, ghc ? nixpkgs.ghc
}:

with nixpkgs.stdenv.lib;

let
  stdenv = nixpkgs.stdenv;

  jdk = nixpkgs.openjdk8;

  jvmlibdir =
    if stdenv.isDarwin
      then "${jdk}/jre/lib/jli"
      else "${jdk}/lib/openjdk/jre/lib/amd64/server";

  leapyearBuildStackProject =
    { extraArgs ? []
    , LD_LIBRARY_PATH ? ""
    , ghc
    , ...
    }@args:

    stdenv.mkDerivation (args // rec {
      buildInputs = [
        nixpkgs.stack
        nixpkgs.nix
        nixpkgs.pkgconfig
        nixpkgs.libiconv
        ghc
      ] ++ stdenv.lib.optional stdenv.isLinux nixpkgs.glibcLocales;

      STACK_PLATFORM_VARIANT="nix";
      STACK_IN_NIX_SHELL=1;
      STACK_IN_NIX_EXTRA_ARGS =
        concatMap (pkg: ["--extra-lib-dirs=${getLib pkg}/lib"
                         "--extra-include-dirs=${getDev pkg}/include"
                        ]) buildInputs
        ++ extraArgs;
    });
in

  leapyearBuildStackProject rec {
    version = "0.2.3.0";
    name = "jni-${version}";
    inherit ghc;
    src = ./.;
    buildInputs = [ jdk ];
    # extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
    LD_LIBRARY_PATH = jvmlibdir;
  }
