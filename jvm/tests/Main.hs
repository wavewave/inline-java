module Main where

import Language.Java (withJVM)
import qualified Spec
import System.Environment (getEnv)
import Test.Hspec

main :: IO ()
main = do
  -- Issuing `withJVM` launches a Java virtual machine, which requires that
  -- the `JAVA_HOME` environment variable be set properly.
  javaHomeEnv <- getEnv "JAVA_HOME"
  putStrLn $ "Invoking `withJVM` with JAVA_HOME=" ++ javaHomeEnv
  withJVM [] $ hspec Spec.spec
