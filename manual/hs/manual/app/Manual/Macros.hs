module Manual.Macros (examples) where

import Foreign as F
import Foreign.C qualified as FC

import Manual.Tools

import Example
import Example.Unsafe

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Macros"

    buffer <- mallocForeignPtrBytes 8
    (x :: Word32) <- withForeignPtr buffer $ \ptr -> do
      poke (plusPtr ptr (fromIntegral fIELD_OFFSET)) (1234 :: Word32)
      peek (pTR_TO_FIELD ptr)
    print x
    print (pTR_TO_FIELD (1 :: FC.CLong))

    year :: YEAR <- alloca $ \ptr -> do
      poke ptr $ Date (YEAR 2025) (MONTH 12) (DAY 25)
      getYear ptr
    print year
