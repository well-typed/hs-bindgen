-- | Create a repository and a first commit, written against the high-level API.
--
-- The write path (init, blob, treebuilder, commit) is the same constructor +
-- managed-handle shape as the read path; the new piece is the 'Signature',
-- written into C by the @asArgumentC sigMarshal@ struct marshaller inside
-- 'commitCreate'. Inputs are fixed so the resulting commit oid is deterministic
-- and matches @CommitLow.hs@ byte for byte.
--
module Main (main) where

import Data.ByteString.Char8 qualified as BS8
import Data.Text.IO qualified as T
import System.Directory (removePathForcibly)
import System.Environment (getArgs)

import LibGit2

demoSig :: Signature
demoSig = Signature
  { sigName  = "Test Author"
  , sigEmail = "test@example.com"
  , sigWhen  = GitTime { gitTimeEpoch = 1700000000, gitTimeOffsetMin = 0 }
  }

main :: IO ()
main = do
  args <- getArgs
  let path = case args of { (p : _) -> p; [] -> "/tmp/libgit2-commit-high" }
  removePathForcibly path
  withNewRepository path $ \repo -> do
    blobOid <- blobCreateFromBuffer repo (BS8.pack "hello, libgit2\n")
    tb      <- treebuilderNew repo
    treebuilderInsert tb "hello.txt" blobOid regularFileMode
    treeOid <- treebuilderWrite tb
    tree    <- treeLookup repo treeOid
    cOid    <- commitCreate repo "HEAD" demoSig demoSig "Initial commit\n" tree
    -- read the commit back
    commit  <- commitLookup repo cOid
    msg     <- commitMessage commit
    author  <- commitAuthor commit
    T.putStrLn ("commit  " <> oidToHex cOid)
    T.putStrLn ("tree    " <> oidToHex treeOid)
    T.putStrLn ("author  " <> sigName author <> " <" <> sigEmail author <> ">")
    T.putStr   ("message " <> msg)
