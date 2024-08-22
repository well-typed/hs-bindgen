module Main (main) where

import Distribution.Simple (defaultMainWithHooks, autoconfUserHooks)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
