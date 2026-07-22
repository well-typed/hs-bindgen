-- | The CSPRNG.
module LibSodium.Random
  ( randomBytes
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CSize)

import HsBindgen.HighLevel (discardResult, dropTrailingUnit, fixed, output,
                            toHighLevel)
import HsBindgen.HighLevel.Marshaller.Utils (byteStringOut)

import Generated.Randombytes.Safe (randombytes_buf)

-- | @n@ cryptographically random bytes from libsodium's CSPRNG
-- (@randombytes_buf@).
randomBytes :: Int -> IO ByteString
randomBytes n =
  toHighLevel
    ( dropTrailingUnit
    $ output (byteStringOut n)         -- void *buf   (out)
    $ fixed  (fromIntegral n :: CSize)  -- size_t size
    $ discardResult
    ) randombytes_buf
