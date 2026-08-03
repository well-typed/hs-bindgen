module Test.Util.QC (
    -- * Arbitrary byte arrays
    byteArrayOf
  , shrinkByteArray
  ) where

import GHC.IsList (IsList (fromList, toList))
import Test.QuickCheck (Gen, shrinkList, vectorOf)

import HsBindgen.Runtime.Support (ByteArray, Word8)

{-------------------------------------------------------------------------------
  Arbitrary byte arrays
-------------------------------------------------------------------------------}

byteArrayOf :: Int -> Gen Word8 -> Gen ByteArray
byteArrayOf n genByte = do
    bytes <- vectorOf n genByte
    pure $ fromList bytes

shrinkByteArray :: (Word8 -> [Word8]) -> ByteArray -> [ByteArray]
shrinkByteArray shrinkByte bytes = [
      fromList bytes'
    | bytes' <- shrinkList shrinkByte (toList bytes)
    ]
