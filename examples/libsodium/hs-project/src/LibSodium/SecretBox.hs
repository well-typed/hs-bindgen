{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Secret-key authenticated encryption (@crypto_secretbox_easy@ /
-- @crypto_secretbox_open_easy@): one shared key, a per-message nonce, and a MAC
-- that authenticates the ciphertext.
--
-- The wrappers show the shape of a libsodium call under the combinators: an
-- explicit 'output' for the ciphertext\/plaintext buffer (@auto@ cannot reach an
-- output buffer), 'HsBindgen.Runtime.HighLevel.input2' for the message and its
-- length, and @input defaultIn@ for the fixed-size key and nonce. 'encrypt' throws
-- on a setup failure; 'open' returns 'Nothing' when authentication fails, since a
-- forged ciphertext is expected input, not an exception.
module LibSodium.SecretBox
  ( -- * Types
    Key (..)
  , Nonce (..)
  , keyBytes
  , nonceBytes
  , macBytes
    -- * Construction
  , newKey
  , randomNonce
  , mkKey
  , mkNonce
    -- * Authenticated encryption
  , encrypt
  , open
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Types (CUChar)

import HsBindgen.Runtime.HighLevel (discardResult, input, input2, output,
                                    resultPure, throwOnNonZero, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (DefaultIn (..))
import HsBindgen.Runtime.HighLevel.Marshaller (at)
import HsBindgen.Runtime.PtrConst (PtrConst)

import Generated.CryptoSecretbox (crypto_secretbox_KEYBYTES,
                                  crypto_secretbox_MACBYTES,
                                  crypto_secretbox_NONCEBYTES)
import Generated.CryptoSecretbox.Safe (crypto_secretbox_easy,
                                       crypto_secretbox_keygen,
                                       crypto_secretbox_open_easy)
import LibSodium.Error (sodiumError)
import LibSodium.Marshal (byteStringOut, bytesConstIn, bytesLenConstIn)
import LibSodium.Random (randomBytes)

-- | Key size in bytes (32), from the generated compile-time constant.
keyBytes :: Int
keyBytes = fromIntegral crypto_secretbox_KEYBYTES

-- | Nonce size in bytes (24).
nonceBytes :: Int
nonceBytes = fromIntegral crypto_secretbox_NONCEBYTES

-- | MAC size in bytes (16). The ciphertext is the plaintext length plus this.
macBytes :: Int
macBytes = fromIntegral crypto_secretbox_MACBYTES

-- | A 32-byte secret key.
newtype Key = Key { unKey :: ByteString }
  deriving stock (Eq, Show)

-- | A 24-byte nonce. Must be unique per message under a given key.
newtype Nonce = Nonce { unNonce :: ByteString }
  deriving stock (Eq, Show)

-- These instances make a 'Key' \/ 'Nonce' a first-class @auto@ input: @input
-- defaultIn@ fills the argument with no per-call marshaller. They are /not/
-- orphans, because this module owns the newtype, unlike libgit2's generated
-- enums whose instances had to sit in a separate @-Wno-orphans@ module.
instance DefaultIn Key where
  type DefInArrow Key lo = PtrConst CUChar -> lo
  defaultIn = at unKey bytesConstIn

instance DefaultIn Nonce where
  type DefInArrow Nonce lo = PtrConst CUChar -> lo
  defaultIn = at unNonce bytesConstIn

-- | Validate a 'ByteString' as a 'Key' (length must be 'keyBytes').
mkKey :: ByteString -> Maybe Key
mkKey bs
  | BS.length bs == keyBytes = Just (Key bs)
  | otherwise                = Nothing

-- | Validate a 'ByteString' as a 'Nonce' (length must be 'nonceBytes').
mkNonce :: ByteString -> Maybe Nonce
mkNonce bs
  | BS.length bs == nonceBytes = Just (Nonce bs)
  | otherwise                  = Nothing

-- | A fresh random key (@crypto_secretbox_keygen@). A single-output
-- @void@-returning fill: 'output' takes the key buffer and 'discardResult' closes
-- the @void@ return. @crypto_secretbox_keygen@ types its argument as
-- @Ptr (Elem (ConstantArray 32 CUChar))@, which 'byteStringOut' fits because it is
-- polymorphic in the pointer element.
newKey :: IO Key
newKey =
  Key . fst <$> toHighLevel
    ( output (byteStringOut keyBytes)  -- unsigned char k[32] (out)
    $ discardResult
    ) crypto_secretbox_keygen

-- | A fresh random nonce.
randomNonce :: IO Nonce
randomNonce = Nonce <$> randomBytes nonceBytes

-- | Encrypt and authenticate @message@ under @key@ and @nonce@
-- (@crypto_secretbox_easy@). The ciphertext is @'macBytes' + length message@
-- bytes. Throws 'SodiumError' only on a setup failure (which @easy@ does not
-- signal in practice).
encrypt :: Key -> Nonce -> ByteString -> IO ByteString
encrypt key nonce message =
  fst <$> toHighLevel
    ( output (byteStringOut (macBytes + BS.length message))  -- c   (out)
    $ input2 bytesLenConstIn                                 -- m, mlen
    $ input  defaultIn                                       -- n   (Nonce)
    $ input  defaultIn                                       -- k   (Key)
    $ throwOnNonZero (sodiumError "crypto_secretbox_easy")
    ) crypto_secretbox_easy message nonce key

-- | Decrypt and verify @ciphertext@ (@crypto_secretbox_open_easy@). 'Nothing'
-- when the MAC does not match (a forged or corrupted ciphertext).
--
-- The output buffer is peeked unconditionally and then discarded on failure: the
-- combinators are per-position, so \"only read the output when the status is 0\"
-- is expressed by hand ('resultPure' keeps the raw status, and 'classify' turns
-- it into 'Maybe'). This is the same shape libgit2's status-conditional iterator
-- needed.
open :: Key -> Nonce -> ByteString -> IO (Maybe ByteString)
open key nonce ciphertext
  | BS.length ciphertext < macBytes = pure Nothing
  | otherwise =
      classify <$> toHighLevel
        ( output (byteStringOut (BS.length ciphertext - macBytes))  -- m (out)
        $ input2 bytesLenConstIn                                    -- c, clen
        $ input  defaultIn                                          -- n
        $ input  defaultIn                                          -- k
        $ resultPure id                                             -- raw status
        ) crypto_secretbox_open_easy ciphertext nonce key
  where
    classify (plaintext, status) =
      if status == 0 then Just plaintext else Nothing
