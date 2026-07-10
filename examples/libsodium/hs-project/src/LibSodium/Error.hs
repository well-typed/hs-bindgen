-- | The libsodium error type and status helpers.
--
-- The hybrid error model: setup, key generation, and signing raise 'SodiumError'
-- (a nonzero status there is a real failure), while authentication and
-- verification return 'Maybe'\/'Bool' (a rejected ciphertext or signature is
-- expected control flow, not an exception). See "LibSodium.SecretBox" and
-- "LibSodium.Sign".
module LibSodium.Error
  ( SodiumError (..)
  , sodiumError
  , checkStatus
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Foreign.C.Types (CInt)

-- | A libsodium call returned a failure status.
data SodiumError = SodiumError
  { sodiumErrorOp     :: String  -- ^ the C function that failed
  , sodiumErrorStatus :: Int     -- ^ its raw return code
  }
  deriving stock (Eq, Show)

instance Exception SodiumError

-- | Build the exception a status closer throws, naming the operation. Pass to
-- 'HsBindgen.Runtime.HighLevel.throwOnNonZero'.
sodiumError :: String -> CInt -> SodiumError
sodiumError op c = SodiumError op (fromIntegral c)

-- | Throw 'SodiumError' if a status code is nonzero. For the hand-written
-- multipart path, where the per-call combinators do not reach (state is threaded
-- across several calls).
checkStatus :: String -> CInt -> IO ()
checkStatus op c = when (c /= 0) (throwIO (sodiumError op c))
