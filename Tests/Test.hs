{-# LANGUAGE TemplateHaskell #-}

module Test (
  loadTestWorking
  ) where

import System.Posix.DynamicLinker.Template
import Language.Haskell.TH

import Foreign.Ptr
import Foreign.C.String
import System.Posix.DynamicLinker.Prim

data TestWorking = TestWorking {
  libHandle ::DL,
  thing1 :: CString -> IO Int,
  thing2 :: Maybe (Int -> Int -> Int),
  thing3 :: Float -> Int,
  thing4 :: Maybe (Float -> IO Double)
}

myModifier :: Maybe (String -> String)
myModifier = Just( (++) "_v2" )

$(makeDynamicLinker ''TestWorking CCall 'myModifier)
