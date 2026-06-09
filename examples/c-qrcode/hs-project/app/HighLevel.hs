{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

-- The DefaultIn instances for the generated enums below are orphans: the enum types
-- and the class are both defined elsewhere. The natural home is the generated module
-- (ideally hs-bindgen would emit them), so silence the expected warning here.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | High-level wrappers around the generated qrcodegen bindings.
--
module HighLevel (
    encodeText
  , getSize
  , getModule
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel (output, scratchArray, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (DefaultIn (..), auto)
import HsBindgen.Runtime.HighLevel.Marshaller (Unmarshaller, scalar)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (peekIncompleteArrayOut)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

-- | Passthrough defaults for the generated enums: each enum is its own C argument
-- type, so the default marshaller is @scalar id@. With these in scope, 'auto' can
-- fill the @ecc@ and @mask@ arguments of 'encodeText'. These are orphan instances;
-- the natural home is alongside the enum in the generated module, so ideally
-- hs-bindgen would emit them.
instance DefaultIn QR.Qrcodegen_Ecc where
  type DefInArrow QR.Qrcodegen_Ecc lo = QR.Qrcodegen_Ecc -> lo
  defaultIn = scalar id

instance DefaultIn QR.Qrcodegen_Mask where
  type DefInArrow QR.Qrcodegen_Mask lo = QR.Qrcodegen_Mask -> lo
  defaultIn = scalar id

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
