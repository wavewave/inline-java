{ mkDerivation, base, binary, bytestring, Cabal, containers
, directory, distributed-closure, filepath, ghc-heap-view, hspec
, inline-c, jni, jvm, language-java, process, singletons, stdenv
, syb, template-haskell, temporary, text, thread-local-storage
, vector
}:
mkDerivation {
  pname = "inline-java";
  version = "0.6.1";
  src = ../.;
  libraryHaskellDepends = [
    base binary bytestring Cabal containers directory
    distributed-closure filepath ghc-heap-view inline-c jni jvm
    language-java process singletons syb template-haskell temporary
    text thread-local-storage vector
  ];
  testHaskellDepends = [
    base bytestring hspec jni jvm singletons text
  ];
  homepage = "http://github.com/tweag/inline-java#readme";
  description = "Java interop via inline Java code in Haskell modules";
  license = stdenv.lib.licenses.bsd3;
}
