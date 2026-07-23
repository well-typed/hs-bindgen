-- | @git log@, written directly against the generated low-level bindings.
--
-- Everything the high-level layer hides is in the open here: explicit
-- @git_libgit2_init@/@shutdown@ on a bound thread, an @alloca@ for every
-- @T **out@ slot, a @peek@ to pull the handle out, @PtrConst@ retagging on every
-- @const@ argument, a manual status check that reads @git_error_last@, manual
-- hex formatting, and a matching @git_*_free@ for every handle. Compare with
-- @LogHigh.hs@: the two print identical output.
--
module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Word (Word8)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.Environment (getArgs)

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Commit.Safe qualified as CS
import Generated.Errors (Git_error (..))
import Generated.Errors.Safe qualified as ES
import Generated.Global.Safe qualified as Gl
import Generated.Oid (Git_oid (..))
import Generated.Refs.Safe qualified as RfS
import Generated.Repository.Safe qualified as RS
import Generated.Revwalk.Safe qualified as WS
import Generated.Types (Git_signature (..))
import Numeric (showHex)

-- | Throw on a negative libgit2 status, reading the thread-local message.
check :: CInt -> IO ()
check n = when (n < 0) $ do
  ep <- ES.git_error_last
  let p = PtrConst.unsafeToPtr ep
  msg <- if p == nullPtr
           then pure "no detail"
           else do Git_error m _ <- peek p
                   if m == nullPtr then pure "" else peekCString m
  ioError (userError ("libgit2 error " ++ show n ++ ": " ++ msg))

oidHexShort :: Int -> Git_oid -> String
oidHexShort k (Git_oid ca) =
    take k $ concatMap hexByte (CA.toList ca)
  where
    hexByte c =
      let s = showHex (fromIntegral c :: Word8) ""
       in if length s == 1 then '0' : s else s

main :: IO ()
main = do
  args <- getArgs
  let path = case args of { (p : _) -> p; [] -> "." }
  runInBoundThread $
    bracket_ (Gl.git_libgit2_init >>= check) (() <$ Gl.git_libgit2_shutdown) $ do
      -- open repository
      repo <- alloca $ \pp -> do
        withCString path $ \cpath ->
          RS.git_repository_open pp (PtrConst.unsafeFromPtr cpath) >>= check
        peek pp
      -- HEAD reference -> branch shorthand
      ref <- alloca $ \pp -> do
        RS.git_repository_head pp repo >>= check
        peek pp
      branch <- peekCString . PtrConst.unsafeToPtr
                  =<< RfS.git_reference_shorthand (PtrConst.unsafeFromPtr ref)
      putStrLn ("On branch " ++ branch)
      RfS.git_reference_free ref
      -- revision walk from HEAD, by commit time
      walk <- alloca $ \pp -> do
        WS.git_revwalk_new pp repo >>= check
        peek pp
      WS.git_revwalk_sorting walk (2 :: CUInt) >>= check    -- GIT_SORT_TIME
      WS.git_revwalk_push_head walk >>= check
      let loop = alloca $ \oidp -> do
            status <- WS.git_revwalk_next oidp walk
            case fromIntegral status :: Int of
              -31 -> pure ()                                 -- GIT_ITEROVER
              0   -> do
                oid <- peek oidp
                commit <- alloca $ \cp -> do
                  CS.git_commit_lookup cp repo (PtrConst.unsafeFromPtr oidp) >>= check
                  peek cp
                sigPtr <- CS.git_commit_author (PtrConst.unsafeFromPtr commit)
                Git_signature namePtr _ _ <- peek (PtrConst.unsafeToPtr sigPtr)
                name <- peekCString namePtr
                summary <- peekCString . PtrConst.unsafeToPtr =<< CS.git_commit_summary commit
                putStrLn (oidHexShort 10 oid ++ "  " ++ name ++ "  " ++ summary)
                CS.git_commit_free commit
                loop
              n   -> check (fromIntegral n)
      loop
      WS.git_revwalk_free walk
      RS.git_repository_free repo
