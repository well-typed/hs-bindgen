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

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import HsBindgen.HighLevel (output, scratchArray, toHighLevel)
import HsBindgen.HighLevel.Defaults (auto)
import HsBindgen.HighLevel.Marshaller (Unmarshaller)
import HsBindgen.HighLevel.Marshaller.Utils (peekIncompleteArrayOut)

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

-- | Lift @qrcodegen_encodeText@. Only the two positions that need a human decision
-- are explicit: @tempBuffer@ is a 'scratchArray' (the callee writes it, the caller
-- never sees it) and @qrcode@ is the 'output' we keep. 'auto' fills the rest from
-- the signature: the leading 'String', then the two enums, the version 'Int's, the
-- boost 'Bool', and the result.
encodeText
  :: String
  -> QR.Qrcodegen_Ecc
  -> Int
  -> Int
  -> QR.Qrcodegen_Mask
  -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel
  ( auto                       -- text (String)
  $ scratchArray @Word8 maxLen -- tempBuffer: written, never read
  $ output  qrCodeOut          -- qrcode: the out-parameter we keep
  $ auto                       -- ecc, minVersion, maxVersion, mask, boostEcl, result
  ) QR.qrcodegen_encodeText
  where
    maxLen :: Int
    maxLen = fromIntegral QR.qrcodegen_BUFFER_LEN_MAX

    qrCodeOut :: Unmarshaller (Ptr Word8) (IncompleteArray Word8)
    qrCodeOut = peekIncompleteArrayOut maxLen

-- | Every position is a default ('IncompleteArray' marshals as a @const@ pointer,
-- 'CInt' \/ 'CBool' scalars), so the whole wrapper is 'auto'.
getSize :: IncompleteArray Word8 -> IO Int
getSize = toHighLevel auto QR.qrcodegen_getSize

getModule :: IncompleteArray Word8 -> Int -> Int -> IO Bool
getModule = toHighLevel auto QR.qrcodegen_getModule
