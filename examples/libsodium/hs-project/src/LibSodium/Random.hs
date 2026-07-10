-- | The CSPRNG.
module LibSodium.Random
  ( randomBytes
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CSize)

import HsBindgen.Runtime.HighLevel (discardResult, output, toHighLevel)

import Generated.Randombytes.Safe (randombytes_buf)
import LibSodium.Marshal (byteStringOut, fixed)

-- | @n@ cryptographically random bytes from libsodium's CSPRNG
-- (@randombytes_buf@).
--
-- Even this single-output @void@-returning fill goes through the combinators:
-- 'output' takes the @void *@ buffer, 'fixed' supplies the @size_t@ length, and
-- 'discardResult' closes the @void@ return. Note the size @n@ appears twice: once
-- to size the 'byteStringOut' allocation and once as the C @size@ argument, since
-- the combinators thread an output's size independently of its buffer. A direct
-- @allocaBytes@ is about as short here; the combinators earn their keep on the
-- multi-argument crypto calls, not on a self-sized single-output fill.
randomBytes :: Int -> IO ByteString
randomBytes n =
  fst <$> toHighLevel
    ( output (byteStringOut n)          -- void *buf   (out)
    $ fixed  (fromIntegral n :: CSize)  -- size_t size
    $ discardResult
    ) randombytes_buf
