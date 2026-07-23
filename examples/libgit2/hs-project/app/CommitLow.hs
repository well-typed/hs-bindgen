-- | Create a repository and a first commit, written directly against the
-- generated bindings. Compare with @CommitHigh.hs@: same output, far more
-- plumbing (an @alloca@ per out-parameter, a @peek@ per handle, @PtrConst@
-- retagging on every @const@ argument, manual @git_*_free@, manual hex, and an
-- explicit @git_signature_new@/@git_signature_free@ pair).
--
module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt, CSize, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek)
import System.Directory (removePathForcibly)
import System.Environment (getArgs)

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Blob.Safe qualified as B
import Generated.Commit.Safe qualified as CS
import Generated.Errors (Git_error (..))
import Generated.Errors.Safe qualified as ES
import Generated.Global.Safe qualified as Gl
import Generated.Oid (Git_oid (..))
import Generated.Repository.Safe qualified as RS
import Generated.Signature.Safe qualified as SS
import Generated.Tree.Safe qualified as TS
import Generated.Types (Git_signature (..), pattern GIT_FILEMODE_BLOB)
import Numeric (showHex)

check :: CInt -> IO ()
check n = when (n < 0) $ do
  ep <- ES.git_error_last
  let p = PtrConst.unsafeToPtr ep
  msg <- if p == nullPtr
           then pure "no detail"
           else do Git_error m _ <- peek p
                   if m == nullPtr then pure "" else peekCString m
  ioError (userError ("libgit2 error " ++ show n ++ ": " ++ msg))

oidHex :: Git_oid -> String
oidHex (Git_oid ca) = concatMap hexByte (CA.toList ca)
  where
    hexByte c =
      let s = showHex (fromIntegral c :: Word8) ""
       in if length s == 1 then '0' : s else s

nullCChar :: PtrConst.PtrConst a
nullCChar = PtrConst.unsafeFromPtr nullPtr

main :: IO ()
main = do
  args <- getArgs
  let path = case args of { (p : _) -> p; [] -> "/tmp/libgit2-commit-low" }
  removePathForcibly path
  runInBoundThread $
    bracket_ (Gl.git_libgit2_init >>= check) (() <$ Gl.git_libgit2_shutdown) $ do
      -- init repository
      repo <- alloca $ \pp -> do
        withCString path $ \cpath ->
          RS.git_repository_init pp (PtrConst.unsafeFromPtr cpath) (0 :: CUInt) >>= check
        peek pp
      -- blob from buffer
      blobOid <- alloca $ \oidp -> do
        BSU.unsafeUseAsCStringLen (BS8.pack "hello, libgit2\n") $ \(p, n) ->
          B.git_blob_create_from_buffer oidp repo
            (PtrConst.unsafeFromPtr (castPtr p)) (fromIntegral n) >>= check
        peek oidp
      -- treebuilder: insert the blob, write the tree
      tb <- alloca $ \pp -> do
        TS.git_treebuilder_new pp repo nullCChar >>= check
        peek pp
      with blobOid $ \oidp ->
        withCString "hello.txt" $ \nm ->
          TS.git_treebuilder_insert nullPtr tb (PtrConst.unsafeFromPtr nm)
            (PtrConst.unsafeFromPtr oidp) GIT_FILEMODE_BLOB >>= check
      treeOid <- alloca $ \oidp -> do
        TS.git_treebuilder_write oidp tb >>= check
        peek oidp
      TS.git_treebuilder_free tb
      tree <- alloca $ \pp -> do
        with treeOid $ \oidp ->
          TS.git_tree_lookup pp repo (PtrConst.unsafeFromPtr oidp) >>= check
        peek pp
      -- signature with a fixed time (deterministic)
      sig <- alloca $ \pp -> do
        withCString "Test Author" $ \nm ->
          withCString "test@example.com" $ \em ->
            SS.git_signature_new pp (PtrConst.unsafeFromPtr nm)
              (PtrConst.unsafeFromPtr em) 1700000000 0 >>= check
        peek pp
      -- commit
      cOid <- alloca $ \oidp -> do
        withCString "HEAD" $ \ref ->
          withCString "Initial commit\n" $ \msg ->
            CS.git_commit_create oidp repo (PtrConst.unsafeFromPtr ref)
              (PtrConst.unsafeFromPtr sig) (PtrConst.unsafeFromPtr sig)
              nullCChar (PtrConst.unsafeFromPtr msg)
              (PtrConst.unsafeFromPtr tree) (0 :: CSize) nullPtr >>= check
        peek oidp
      SS.git_signature_free sig
      -- read the commit back
      commit <- alloca $ \pp -> do
        with cOid $ \oidp ->
          CS.git_commit_lookup pp repo (PtrConst.unsafeFromPtr oidp) >>= check
        peek pp
      msg <- peekCString . PtrConst.unsafeToPtr =<< CS.git_commit_message (PtrConst.unsafeFromPtr commit)
      sigPtr <- CS.git_commit_author (PtrConst.unsafeFromPtr commit)
      Git_signature namePtr emailPtr _ <- peek (PtrConst.unsafeToPtr sigPtr)
      name  <- peekCString namePtr
      email <- peekCString emailPtr
      putStrLn ("commit  " ++ oidHex cOid)
      putStrLn ("tree    " ++ oidHex treeOid)
      putStrLn ("author  " ++ name ++ " <" ++ email ++ ">")
      putStr   ("message " ++ msg)
      CS.git_commit_free commit
      TS.git_tree_free tree
      RS.git_repository_free repo
