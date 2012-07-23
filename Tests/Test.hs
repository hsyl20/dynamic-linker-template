{-# LANGUAGE TemplateHaskell #-}

module Test (
  loadTestWorking
  ) where

import System.Posix.DynamicLinker.Template

import Foreign.C.String

data TestWorking = TestWorking {
  libHandle ::DL,
  thing1 :: CString -> IO Int,
  thing2 :: Maybe (Int -> Int -> Int),
  thing3 :: Float -> Int,
  thing4 :: Maybe (Float -> IO Double)
}

myModifier :: String -> String
myModifier = (++ "_v2")

$(makeDynamicLinker ''TestWorking CCall 'myModifier)
