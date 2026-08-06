-- | High-level wrappers around the generated qrcodegen bindings.
--
module HighLevel (
    encodeText
  , getSize
  , getModule
  ) where

import Data.Word (Word8)

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import HsBindgen.HighLevel (input, output, scratchArray, toHighLevel,
                            toHighLevelPure)
import HsBindgen.HighLevel.Auto (auto)
import HsBindgen.HighLevel.Defaults (defaultIn)
import HsBindgen.HighLevel.Marshaller.Utils (peekIncompleteArrayOut)

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

-- | Lift @qrcodegen_encodeText@. Only the two positions that need a human decision
-- are written out: @tempBuffer@ is a 'scratchArray' (the callee writes it, we never
-- look) and @qrcode@ is the 'output' we keep. Nothing else in that C signature is a
-- judgement call, so 'auto' takes over: the five remaining inputs, and then the
-- result, which it builds as the output followed by the converted @bool@ return.
--
-- The leading @text@ argument is spelled out only because 'auto' runs to the end of
-- the spec once it starts, so anything before an explicit position has to be.
--
-- This one stays in 'IO', unlike the two below. The encoder is deterministic, but the
-- @qrcode@ buffer is allocated at @qrcodegen_BUFFER_LEN_MAX@ and C writes only the
-- bytes the chosen version needs, so the array read back has an uninitialised tail.
-- Nothing here ever looks at it, and 'getSize' bounds every read, but the returned
-- value is still not a function of the arguments alone, which is what
-- 'HsBindgen.HighLevel.toHighLevelPure' would be asserting.
encodeText
  :: String
  -> QR.Qrcodegen_Ecc
  -> Int
  -> Int
  -> QR.Qrcodegen_Mask
  -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel
  ( input defaultIn         -- text (String)
  $ scratchArray maxLen     -- tempBuffer: written, never read
  $ output qrCodeOut        -- qrcode: the out-parameter we keep
  $ auto                    -- ecc, minVersion, maxVersion, mask, boostEcl,
  ) QR.qrcodegen_encodeText -- then (qrcode, ok)
  where
    maxLen    = fromIntegral QR.qrcodegen_BUFFER_LEN_MAX
    qrCodeOut = peekIncompleteArrayOut maxLen

-- | Every position is a default ('IncompleteArray' marshals as a @const@ pointer,
-- 'CInt' \/ 'CBool' scalars), so the whole spec is 'auto'.
--
-- Both of these read a finished code and compute: no allocation, no global state, and
-- an 'IncompleteArray' is an immutable owned buffer, so the same code always gives the
-- same answer. 'toHighLevelPure' says so and takes the 'IO' off, which is what lets
-- 'Main.printQr' read a module inside 'Data.Foldable.for_' without a bind.
getSize :: IncompleteArray Word8 -> Int
getSize = toHighLevelPure auto QR.qrcodegen_getSize

getModule :: IncompleteArray Word8 -> Int -> Int -> Bool
getModule = toHighLevelPure auto QR.qrcodegen_getModule
