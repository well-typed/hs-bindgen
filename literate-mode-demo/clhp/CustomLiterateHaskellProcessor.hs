module CustomLiterateHaskellProcessor (main) where

import System.Environment
import System.Exit

main :: IO ()
main = do
    -- Not sure if this is documented anywhere..? Not that I could find.
    -- But I think the arguments are as expected by @unlit@
    -- <https://gitlab.haskell.org/ghc/ghc/-/blob/master/utils/unlit/unlit.c>
    args <- getArgs
    case args of
      ["-h", _label, input, output] ->
        run input output
      _otherwise -> do
        putStrLn $ "Unexpected arguments " ++ show args
        exitFailure

-- For this demo, we ignore the input entirely and just generate an empty
-- Haskell file
run :: FilePath -> FilePath -> IO ()
run _input output =
    writeFile output $ unlines [
        "module SomeRandomFile where"
      , "foo :: Int"
      , "foo = 1"
      ]

