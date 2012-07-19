{-# LANGUAGE TemplateHaskell #-}

module Test (
  loadTestWorking
  ) where

import System.Posix.DynamicLinker.Template
import Language.Haskell.TH

import Foreign.Ptr
import System.Posix.DynamicLinker.Prim (DL)

data TestWorking = TestWorking {
  libHandle ::DL,
  thing3 :: Int -> Int -> Int,
  thing4 :: Float -> Int
}

$(makeDynamicLinker ''TestWorking)

{-data TestMissingDL = TestMissingDL {
  thing :: Int -> Int -> Int,
  thing2 :: Float -> Int
}

$(makeDynamicLinker ''TestMissingDL)-}

{-data TestOptional = TestOptional {
  thing5 :: Int -> Int -> Int,
  optionalThing :: Maybe (Float -> Int),
  thing6 :: Float -> Int
}

$(makeDynamicLinker ''TestOptional)-}
