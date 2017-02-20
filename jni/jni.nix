{ stdenv
, jdk
, mkDerivation
, cpphs
, base, bytestring, containers
, inline-c, singletons, thread-local-storage, choice
}:
let
  jvmlibdir =
    if stdenv.isDarwin
      then "${jdk}/jre/lib/jli"
      else "${jdk}/lib/openjdk/jre/lib/amd64/server";
in
mkDerivation {
  pname = "jni";
  version = "0.2.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers inline-c singletons thread-local-storage choice
    jdk
  ];
  libraryToolDepends = [ cpphs ];

  # We need this because libjli.dylib is in a nonstandard location relative
  # to its package base directory. We get:
  # ```
  # Setup: Missing dependency on a foreign library:
  # * Missing C library: jli
  # ```
  configureFlags = [
    "--extra-lib-dirs=${jvmlibdir}"
    "--ghc-option=-optl=-Wl,-rpath,${jvmlibdir}"
  ];

  # We need this because the _hsc_make binaries can't otherwise find
  # `@rpath/libjli.dylib`.
  #
  preBuild = ''
    export DYLD_LIBRARY_PATH=${jvmlibdir}:$DYLD_LIBRARY_PATH
  '';

  # MacOS restricts the set of paths that are searched for @rpath dynamic
  # libraries, and our libjli.dylib is not in the restricted set, in some
  # circumstances.
  #
#  preFixup = if stdenv.isDarwin then ''
#    install_name_tool -change \
#      @rpath/libjli.dylib \
#      ${jvmlibdir}/libjli.dylib \
#      $out/lib/ghc-8.*/x86_64-osx-ghc-8.*/libHSjni-*.dylib
#  '' else "";

  homepage = "https://github.com/tweag/inline-java/tree/master/jni#readme";
  description = "Complete JNI raw bindings";
  license = stdenv.lib.licenses.bsd3;
}
