{ mkDerivation, base, bytestring, distributed-closure, hspec
, singletons, stdenv, text, vector
, jni
}:
mkDerivation {
  pname = "jvm";
  version = "0.2.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring distributed-closure jni singletons text vector
  ];
  testHaskellDepends = [ base bytestring hspec text ];

  homepage = "http://github.com/tweag/inline-java/tree/master/jvm#readme";
  description = "Call JVM methods from Haskell";
  license = stdenv.lib.licenses.bsd3;
}
