module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import System.Environment (getArgs)

import AnsiDiff (ansidiff)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [old, new] -> main' old new
        _          -> return ()

main' :: FilePath -> FilePath -> IO ()
main' old new = do
    old' <- readFileUTF8 old
    new' <- readFileUTF8 new
    putStr $ ansidiff old' new'

readFileUTF8 :: FilePath -> IO String
readFileUTF8 fp = do
    contents <- BS.readFile fp
    return $ UTF8.toString contents
