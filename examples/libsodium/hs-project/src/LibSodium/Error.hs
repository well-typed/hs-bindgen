-- | The libsodium error type and the helper that builds it.
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

import Control.Exception (Exception)
import Foreign.C.Types (CInt)

import HsBindgen.HighLevel (throwUnlessZero)

-- | A libsodium call returned a failure status.
data SodiumError = SodiumError
  { sodiumErrorOp     :: String  -- ^ the C function that failed
  , sodiumErrorStatus :: Int     -- ^ its raw return code
  }
  deriving stock (Eq, Show)

instance Exception SodiumError

-- | Build the exception, naming the operation. Pass it to
-- 'HsBindgen.HighLevel.throwOnNonZero', which closes a spec that has no outputs.
sodiumError :: String -> CInt -> SodiumError
sodiumError op c = SodiumError op (fromIntegral c)

-- | Throw 'SodiumError' when @op@ returned a non-zero status.
--
-- 'sodiumError' as a check rather than a closer, which is the shape the combinators
-- want wherever the status guards an out-parameter:
--
-- > autoChecked (checkStatus "crypto_secretbox_easy")
checkStatus :: String -> CInt -> IO ()
checkStatus = throwUnlessZero . sodiumError
