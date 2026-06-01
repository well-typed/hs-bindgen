{-# LANGUAGE TypeApplications #-}

-- | High-level wrappers around the generated qrcodegen bindings.
--
module HighLevel (
    encodeText
  , getSize
  , getModule
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel.ToHighLevel (OutMarshaller, input, output,
                                                pureIn, scratchArray,
                                                toHighLevel)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Defaults (defaultIn, defaultRes)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers (peekIncompleteArrayOut,
                                                            withCStringIn,
                                                            withConstIncompleteArrayIn)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

-- | Lift @qrcodegen_encodeText@. @tempBuffer@ is a 'scratchArray', so it adds
-- nothing to the result: only the QR code and the success flag come back. The
-- scalar inputs and the result use default marshallers.
encodeText
  :: String
  -> QR.Qrcodegen_Ecc
  -> Int
  -> Int
  -> QR.Qrcodegen_Mask
  -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel
  ( input   withCStringIn
  $ scratchArray @Word8 maxLen -- tempBuffer: written, never read
  $ output  qrCodeOut
  $ input   (pureIn id)        -- ecc level (enum, passthrough)
  $ input   defaultIn          -- minVersion (Int -> CInt)
  $ input   defaultIn          -- maxVersion (Int -> CInt)
  $ input   (pureIn id)        -- mask (enum, passthrough)
  $ input   defaultIn          -- boostEcl (Bool -> CBool)
  $ defaultRes                 -- CBool -> Bool
  ) QR.qrcodegen_encodeText
  where
    maxLen :: Int
    maxLen = fromIntegral QR.qrcodegen_BUFFER_LEN_MAX

    qrCodeOut :: OutMarshaller (Ptr Word8) (IncompleteArray Word8)
    qrCodeOut = peekIncompleteArrayOut maxLen

getSize :: IncompleteArray Word8 -> IO Int
getSize = toHighLevel
  ( input  withConstIncompleteArrayIn
  $ defaultRes
  ) QR.qrcodegen_getSize

getModule :: IncompleteArray Word8 -> Int -> Int -> IO Bool
getModule = toHighLevel
  ( input  withConstIncompleteArrayIn
  $ input  defaultIn                  -- x (Int -> CInt)
  $ input  defaultIn                  -- y (Int -> CInt)
  $ defaultRes                 -- CBool -> Bool
  ) QR.qrcodegen_getModule
