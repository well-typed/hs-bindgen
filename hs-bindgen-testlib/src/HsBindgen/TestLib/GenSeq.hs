module HsBindgen.TestLib.GenSeq (
    -- * GenSeq
    GenSeq (..)
  , genSeq
  ) where

import Foreign.C qualified as FC

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Ordinal value of the letter @A@
ordA :: Word
ordA = 65

{-------------------------------------------------------------------------------
  GenSeq
-------------------------------------------------------------------------------}

class GenSeq a where
  genSeqStep :: Word -> (a, Word)

instance GenSeq FC.CChar where
  genSeqStep w = (fromIntegral (w + ordA), w + 1)

instance GenSeq FC.CSChar where
  genSeqStep w = (fromIntegral (w + ordA), w + 1)

instance GenSeq FC.CUChar where
  genSeqStep w = (fromIntegral (w + ordA), w + 1)

instance GenSeq FC.CShort where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CUShort where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CInt where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CUInt where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CLong where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CULong where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CPtrdiff where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CSize where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CWchar where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CSigAtomic where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CLLong where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CULLong where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CBool where
  genSeqStep w = (fromIntegral (w `mod` 2), w + 1)

instance GenSeq FC.CIntPtr where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CUIntPtr where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CIntMax where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CUIntMax where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CClock where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CTime where
  genSeqStep w = (fromIntegral w, w + 1)

{- TODO remove or fix
instance GenSeq FC.CUSeconds where
  genSeqStep w = (fromIntegral w, w + 1)
-}

instance GenSeq FC.CSUSeconds where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CFloat where
  genSeqStep w = (fromIntegral w, w + 1)

instance GenSeq FC.CDouble where
  genSeqStep w = (fromIntegral w, w + 1)

genSeq :: GenSeq a => a
genSeq = fst $ genSeqStep 1
