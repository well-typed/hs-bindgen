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
  ) where

import Control.Exception (Exception)
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
