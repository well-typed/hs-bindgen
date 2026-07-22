-- | Pure views of the deterministic libsodium primitives, for @import qualified@.
--
-- Each function mirrors its 'IO' counterpart in "LibSodium.SecretBox" \/
-- "LibSodium.Sign" under the same name, with the 'IO' dropped by 'assertPure'
-- (@unsafePerformIO@). The primitives are deterministic functions of their
-- inputs, so this is sound. Intended use:
--
-- > import LibSodium
-- > import LibSodium.Pure qualified as Pure
-- >
-- > let ciphertext = Pure.encrypt key nonce message
--
-- Precondition: libsodium must be initialised ('LibSodium.Init.sodiumInit')
-- before any function here is forced; this module does not initialise for you.
--
-- Partiality: 'encrypt', 'signDetached', and 'signMultipart' inherit their 'IO'
-- counterpart's exception, thrown from pure code on a setup failure
-- ('LibSodium.Error.SodiumError'). 'open', 'verifyDetached', and
-- 'verifyMultipart' are total.
module LibSodium.Pure
  ( encrypt
  , open
  , seedKeypair
  , signDetached
  , verifyDetached
  , signMultipart
  , verifyMultipart
  ) where

import Data.ByteString (ByteString)

import HsBindgen.HighLevel (assertPure)

import LibSodium.SecretBox (Key, Nonce)
import LibSodium.SecretBox qualified as SecretBox
import LibSodium.Sign (PublicKey, SecretKey, Seed, Signature)
import LibSodium.Sign qualified as Sign

-- | 'LibSodium.SecretBox.encrypt' as a pure function.
encrypt :: Key -> Nonce -> ByteString -> ByteString
encrypt = assertPure SecretBox.encrypt

-- | 'LibSodium.SecretBox.open' as a pure function.
open :: Key -> Nonce -> ByteString -> Maybe ByteString
open = assertPure SecretBox.open

-- | 'LibSodium.Sign.seedKeypair' as a pure function (deterministic in the seed).
seedKeypair :: Seed -> (PublicKey, SecretKey)
seedKeypair = assertPure Sign.seedKeypair

-- | 'LibSodium.Sign.signDetached' as a pure function (Ed25519 signing is
-- deterministic).
signDetached :: SecretKey -> ByteString -> Signature
signDetached = assertPure Sign.signDetached

-- | 'LibSodium.Sign.verifyDetached' as a pure function.
verifyDetached :: PublicKey -> Signature -> ByteString -> Bool
verifyDetached = assertPure Sign.verifyDetached

-- | 'LibSodium.Sign.signMultipart' as a pure function.
signMultipart :: SecretKey -> [ByteString] -> Signature
signMultipart = assertPure Sign.signMultipart

-- | 'LibSodium.Sign.verifyMultipart' as a pure function.
verifyMultipart :: PublicKey -> Signature -> [ByteString] -> Bool
verifyMultipart = assertPure Sign.verifyMultipart
