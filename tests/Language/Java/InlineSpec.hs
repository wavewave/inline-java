{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.InlineSpec where

import qualified Data.Coerce as Coerce
import Data.Int
import qualified Data.Vector.Storable as VS
import Foreign (Ptr, FunPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.JNI as JNI
import Foreign.JNI.Types
import GHC.Stats
import Language.Java
import Language.Java.Inline
import Data.Singletons (SomeSing(..))
import Test.Hspec

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import System.Mem

foreign export ccall "hsApply" hsApply
  :: JNIEnv -> Ptr JObject -> Ptr JIntArray -> IO (Ptr JIntArray)
foreign import ccall "&hsApply" hsApplyPtr
  :: FunPtr (JNIEnv -> Ptr JObject -> Ptr JIntArray -> IO (Ptr JIntArray))

countRef :: IORef Int
countRef = unsafePerformIO $ newIORef 0
{-# NOINLINE countRef #-}

hsApply :: JNIEnv -> Ptr JObject -> Ptr JIntArray -> IO (Ptr JIntArray)
hsApply _ _ ptr = do
    jarg <- J <$> newForeignPtr_ ptr
    v <- reify (unsafeCast jarg) :: IO (VS.Vector Int32)
    x <- atomicModifyIORef' countRef (\x -> (x+1, x))
    when (mod x 1000000 == 0) $ do
      performMajorGC
      getGCStats >>= print
    unsafeForeignPtrToPtr <$> Coerce.coerce <$> reflect v

spec :: Spec
spec = do
    describe "Java quasiquoter" $ do
      it "Evaluates simple expressions" $ do
        ([java| 1 + 1 |] >>= reify) `shouldReturn` (2 :: Int32)

      it "Evaluates simple blocks" $ do
        ([java| {
             int x = 1;
             int y = 2;
             return x + y;
           } |] >>= reify) `shouldReturn` (3 :: Int32)

      it "Supports antiquotation variables" $ do
        let x = 1 :: Int32
        ([java| $x + 1 |] >>= reify) `shouldReturn` (2 :: Int32)

      it "Supports multiple antiquotation variables" $ do
        let foo = 1 :: Int32
            bar = 2 :: Int32
        ([java| $foo + $bar |] >>= reify) `shouldReturn` (3 :: Int32)

      it "Supports antiquotation variables in blocks" $ do
        let z = 1 :: Int32
        ([java| { return $z + 1; } |] >>= reify) `shouldReturn` (2 :: Int32)

      it "Supports anonymous classes" $ do
        _ :: JObject <- [java| new Object() {} |]
        return ()

      it "Supports multiple anonymous classes" $ do
        ([java| new Object() {}.equals(new Object() {}) |] >>= reify) `shouldReturn` False

      it "doesn't leak" $ (do
        jf <- [java|
            new java.util.function.Function<int[],int[]>() {
              @Override
              public int[] apply(int[] d) { return hsApply(d); }
              private native int[] hsApply(int[] d);
            }
          |] :: IO (J ('Class "java.util.function.Function"))

        klass <- JNI.getObjectClass jf
        JNI.registerNatives klass
          [ JNI.JNINativeMethod
              "hsApply"
              (methodSignature [SomeSing (sing :: Sing ('Array ('Prim "int")))]
                               (sing :: Sing ('Array ('Prim "int")))
              )
              hsApplyPtr
          ]

        r <- [java| {
           int[] prev = new int[100];
           Runtime r = Runtime.getRuntime();
           for(int i=0;i<100000000;i++) {
             prev = (int[])$jf.apply(prev);
             if (i % 1000000 == 0) {
               try { Thread.sleep(1000); } catch(Exception ex) {}
               System.gc();
               long live = r.totalMemory() - r.freeMemory();
               System.out.println("live bytes: " + live);
               System.out.println("heap size: " + r.totalMemory());
             }
           };
           return 0;
           }
          |] >>= reify :: IO Int32
        return r
          ) `shouldReturn` 0
