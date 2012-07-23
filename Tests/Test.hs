{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Test (loadMyLib, MyLib(..)) where

import System.Posix.DynamicLinker.Template

data MyLib = MyLib {
  libHandle ::DL,
  thing1 :: Double -> IO Int,          -- Mandatory symbol
  thing2 :: Maybe (Int -> Int -> Int)  -- Optional symbol
}

myModifier :: String -> String
myModifier = (++ "_v2")

$(makeDynamicLinker ''MyLib CCall 'myModifier)

-- Load your library with:
-- loadMyLib :: FilePath -> [RTLDFlags] -> IO MyLib
