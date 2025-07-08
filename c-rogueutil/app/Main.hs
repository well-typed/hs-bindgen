{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified RogueUtil as RU
import Foreign.C (newCString)
import Formatting

main :: IO ()
main = do
  RU.setColor (fromIntegral $ RU.un_Color_code RU.YELLOW)
  pressAnyKeyString <- newCString "Press any key to start"
  RU.anykey (RU.RUTIL_STRING pressAnyKeyString)
  RU.resetColor
  RU.colorPrint RU.RED RU.WHITE ("\nThis is the manual colorPrint binding: " % string % int) "ola" 2
