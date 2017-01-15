{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~))
import Language.Java (withJVM)
import Foreign.JNI (configGcTorture, configJvmOptions, defaultConfig)
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM config $ hspec Spec.spec
  where
    config =
      defaultConfig &
        configJvmOptions .~ ["-Xcheck:jni"] &
        configGcTorture .~ True
