{-# LANGUAGE FlexibleContexts #-}

-- | libgit2's error model as Haskell exceptions.
--
-- libgit2 reports failure as a negative @int@ return code, with the human
-- message and category in thread-local storage reached through
-- @git_error_last@. We surface that as a single 'GitError' exception.
--
module LibGit2.Error
  ( GitError (..)
  , gitError
  , checkStatus
  , checkedStatus
  ) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (ToHighLevel)
import HsBindgen.HighLevel.Auto (AutoOutputs, checkedResult)

import Generated.Errors (Git_error (..))
import Generated.Errors.Safe (git_error_last)

-- | A failed libgit2 call: the negative return code, the error category
-- (@klass@), and the last error message.
data GitError = GitError
  { gitErrorCode    :: Int
  , gitErrorKlass   :: Int
  , gitErrorMessage :: Text
  }
  deriving (Eq, Show)

instance Exception GitError

-- | Read libgit2's thread-local last error and pair it with a status code.
--
-- This MUST run on the same OS thread as the failing call. The high-level entry
-- points ("LibGit2.Git") run under 'Control.Concurrent.runInBoundThread' so the
-- RTS keeps the failing call and this read on one OS thread; otherwise a @safe@
-- FFI call can migrate and this would read another thread's (empty) slot.
gitError :: CInt -> IO GitError
gitError code = do
  ep <- git_error_last
  let p = PtrConst.unsafeToPtr ep
  if p == nullPtr
    then pure (GitError (fromIntegral code) 0 (T.pack "no libgit2 error detail"))
    else do
      Git_error msgPtr klass <- peek p
      msg <- if msgPtr == nullPtr then pure T.empty else T.pack <$> peekCString msgPtr
      pure (GitError (fromIntegral code) (fromIntegral klass) msg)

-- | Throw 'GitError' on a negative libgit2 status; @()@ otherwise.
checkStatus :: CInt -> IO ()
checkStatus n
  | n < 0     = throwIO =<< gitError n
  | otherwise = pure ()

-- | The closer for a fallible libgit2 call: throw on a negative status, otherwise
-- return whatever the call wrote into its out-parameters.
--
-- One definition serves every arity: no out-parameters closes to @IO ()@, one to
-- @IO a@, two to @IO (a, b)@.
--
-- The check is 'IO' because libgit2 keeps the error /detail/ in thread-local state
-- reached through @git_error_last@, which a pure classifier cannot read.
--
-- The status is checked before any out-parameter is read back, which matters here:
-- 'LibGit2.Marshal.outHandle' wraps its slot in a 'Foreign.ForeignPtr.ForeignPtr'
-- with a @git_X_free@ finaliser, and a failed call never wrote that slot.
checkedStatus :: AutoOutputs os hs => ToHighLevel os (IO CInt) (IO hs)
checkedStatus = checkedResult checkStatus
