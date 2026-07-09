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
  , checkStatusResult
  ) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.HighLevel (ToHighLevel, resultIO)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

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

-- | A 'ToHighLevel' closer that consumes the C @int@ status: throws on failure,
-- yields @()@ on success. Closes most fallible libgit2 wrappers.
--
-- It is 'resultIO', not the runtime's 'HsBindgen.Runtime.HighLevel.throwOnNonZero':
-- the error /detail/ is thread-local state reached through an 'IO' call
-- (@git_error_last@), which a pure @c -> e@ classifier cannot read.
checkStatusResult :: ToHighLevel (IO CInt) (IO ())
checkStatusResult = resultIO checkStatus
