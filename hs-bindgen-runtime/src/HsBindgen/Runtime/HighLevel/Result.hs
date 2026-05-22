-- | Result-side helpers for C conventions that 'hl' cannot infer from
-- the user's result type alone (out parameters, output buffers).
--
-- Both combinators return @(payload, foreign-return-value)@ — payload
-- first, foreign call's status second.
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.HighLevel.Result qualified as Result
module HsBindgen.Runtime.HighLevel.Result (
    -- * Single out parameter
    withOut
    -- * Buffer with length pointer
  , withBuf
  ) where

import Foreign.C.Types (CSize)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (peek, poke, sizeOf))

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA

-- | Allocate a single out parameter, run the C call, peek the value.
--
-- > hsGetSize :: IO Int
-- > hsGetSize = do
-- >   (csize, _status) <- withOut get_size
-- >   pure (fromIntegral csize)
withOut :: Storable c => (Ptr c -> IO r) -> IO (c, r)
withOut k = alloca $ \p -> do
  r <- k p
  c <- peek p
  pure (c, r)

-- | Allocate a buffer of @capacity@ elements and a length pointer
-- pre-poked with @capacity@. The c2hs @+@ pattern: C reads the
-- capacity, writes back the actually-used length, and the buffer is
-- read out as an 'IncompleteArray' of that length.
--
-- > srp6Step1 ... cap = do
-- >   (arr, _status) <- withBuf cap $ \pBuf pLen ->
-- >     low_level_call ... pBuf pLen
-- >   pure (B arr)
withBuf
  :: forall a r. Storable a
  => Int                                   -- ^ buffer capacity in elements
  -> (Ptr a -> Ptr CSize -> IO r)
  -> IO (IncompleteArray a, r)
withBuf cap k = do
  let bytes = cap * sizeOf (undefined :: a)
  allocaBytes bytes $ \pBuf ->
    alloca $ \pLen -> do
      poke pLen (fromIntegral cap)
      r <- k pBuf pLen
      n <- peek pLen
      arr <- IA.peekArray (fromIntegral n) (IA.toPtr pBuf)
      pure (arr, r)
