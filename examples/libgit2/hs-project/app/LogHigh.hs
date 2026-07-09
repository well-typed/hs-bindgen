-- | @git log@, written against the high-level API.
--
-- The repository handle and the libgit2 lifecycle are hidden by the @Git@ monad
-- ('withRepository'); the per-call wrappers return idiomatic 'Data.Text.Text',
-- 'Oid', and 'Signature'; errors are exceptions. Compare with @LogLow.hs@.
--
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment (getArgs)

import LibGit2

main :: IO ()
main = do
  args <- getArgs
  let path = case args of { (p : _) -> p; [] -> "." }
  withRepository path $ do
    branch <- headReference >>= liftIO . referenceShorthand
    liftIO $ T.putStrLn ("On branch " <> branch)
    oids <- walkFromHead
    forM_ oids $ \oid -> do
      c  <- lookupCommit oid
      ln <- liftIO $ do
        author  <- commitAuthor c
        summary <- commitSummary c
        pure $ T.concat [oidToHexShort 10 oid, "  ", sigName author, "  ", summary]
      liftIO $ T.putStrLn ln
