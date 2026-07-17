module Main (main) where

import qualified First
import qualified Second

main :: IO ()
main = do
  firstHandle <- First.initialize
  secondHandle <- Second.initialize firstHandle
  print firstHandle
  print secondHandle
