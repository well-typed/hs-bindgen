-- | High-level wrappers around the generated qrcodegen bindings.
module HighLevel (
    encodeText
  , getSize
  , getModule
  ) where

import Data.Word (Word8)
import Foreign.Marshal.Utils qualified as Marshal
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel.Refine (OutMarshaller, input, output,
                                           peekIncompleteArrayOut, pureIn,
                                           pureRes, result, scratchOut,
                                           toHighLevel, withCStringIn,
                                           withConstIncompleteArrayIn)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

-- | Lift @qrcodegen_encodeText@. Destructure the leading scratch @()@ at
-- the call site.
encodeText
  :: String
  -> QR.Qrcodegen_Ecc
  -> Int
  -> Int
  -> QR.Qrcodegen_Mask
  -> Bool
  -> IO ((), (IncompleteArray Word8, Bool))
encodeText = toHighLevel
  ( input  withCStringIn
  $ output tempScratch
  $ output qrCodeOut
  $ input  (pureIn id)
  $ input  (pureIn fromIntegral)
  $ input  (pureIn fromIntegral)
  $ input  (pureIn id)
  $ input  (pureIn Marshal.fromBool)
  $ result (pureRes Marshal.toBool)
  ) QR.qrcodegen_encodeText
  where
    maxLen :: Int
    maxLen = fromIntegral QR.qrcodegen_BUFFER_LEN_MAX

    tempScratch :: OutMarshaller (Ptr Word8) ()
    tempScratch = scratchOut maxLen

    qrCodeOut :: OutMarshaller (Ptr Word8) (IncompleteArray Word8)
    qrCodeOut = peekIncompleteArrayOut maxLen

getSize :: IncompleteArray Word8 -> IO Int
getSize = toHighLevel
  ( input  withConstIncompleteArrayIn
  $ result (pureRes fromIntegral)
  ) QR.qrcodegen_getSize

getModule :: IncompleteArray Word8 -> Int -> Int -> IO Bool
getModule = toHighLevel
  ( input  withConstIncompleteArrayIn
  $ input  (pureIn fromIntegral)
  $ input  (pureIn fromIntegral)
  $ result (pureRes Marshal.toBool)
  ) QR.qrcodegen_getModule
