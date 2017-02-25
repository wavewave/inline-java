{ nixpkgs ? import ./nix/nixpkgs.nix
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
    { ghc
    , buildInputs ? []
    , extraArgs ? []
    , LD_LIBRARY_PATH ? ""
    , ...
    }@args:

    stdenv.mkDerivation (args // {
      buildInputs = args.buildInputs ++ [
        nixpkgs.stack
        nixpkgs.nix
        nixpkgs.pkgconfig
        nixpkgs.libiconv
        ghc
      ] ++ optional stdenv.isLinux nixpkgs.glibcLocales;

      STACK_PLATFORM_VARIANT="nix";
      STACK_IN_NIX_SHELL=1;
      STACK_IN_NIX_EXTRA_ARGS =
        concatMap (pkg: ["--extra-lib-dirs=${getLib pkg}/lib"
                         "--extra-include-dirs=${getDev pkg}/include"
                        ])
                  args.buildInputs
        ++ extraArgs;

      preferLocalBuild = true;
    });
in
  leapyearBuildStackProject rec {
    version = "0.6.1.0";
    name = "inline-java-${version}";
    inherit ghc;
    # src = ./.;
    buildInputs = [ jdk ];
    extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
    LD_LIBRARY_PATH = jvmlibdir;
  }
