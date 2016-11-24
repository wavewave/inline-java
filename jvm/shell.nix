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
    { buildInputs ? []
    , extraArgs ? []
    , LD_LIBRARY_PATH ? ""
    , ghc
    , ...
    }@args:

    stdenv.mkDerivation (args // {

      LD_LIBRARY_PATH =
        args.LD_LIBRARY_PATH + ":" + makeLibraryPath buildInputs;

      buildInputs =
        buildInputs ++ [
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

      preferLocalBuild = true;

      configurePhase = ''
        export STACK_ROOT=$NIX_BUILD_TOP/.stack
        stack setup
      '';

      buildPhase = ''
        stack build
      '';

      checkPhase = ''
        runHook preCheck
        stack test
        runHook postCheck
      '';

      installPhase = ''
        mkdir -p $out/bin
        stack --local-bin-path=$out/bin build --copy-bins
      '';
    });
in

  leapyearBuildStackProject {
    name = "jvm-0.2.3.0";
    version = "0.2.3.0";
    inherit ghc;
    # src = ./.;
    buildInputs = [ jdk ];
    extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
    LD_LIBRARY_PATH = jvmlibdir;
  }
