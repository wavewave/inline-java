module Main where

import Language.Java (defaultConfig, withJVM)
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM defaultConfig $ hspec Spec.spec
