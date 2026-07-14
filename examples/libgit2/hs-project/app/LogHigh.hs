-- | @git log@, written against the high-level API.
--
-- 'withRepository' opens the repository and hands it to the callback; the libgit2
-- lifecycle and the bound-thread requirement live inside it. The per-call wrappers
-- return idiomatic 'Data.Text.Text', 'Oid', and 'Signature'; errors are exceptions.
-- Compare with @LogLow.hs@.
--
module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

import LibGit2

main :: IO ()
main = do
  args <- getArgs
  let path = case args of { (p : _) -> p; [] -> "." }
  withRepository path $ \repo -> do
    branch <- referenceShorthand =<< repositoryHead repo
    T.putStrLn ("On branch " <> branch)
    oids <- walkFromHead repo
    forM_ oids $ \oid -> do
      c       <- commitLookup repo oid
      author  <- commitAuthor c
      summary <- commitSummary c
      T.putStrLn $ T.concat [oidToHexShort 10 oid, "  ", sigName author, "  ", summary]
