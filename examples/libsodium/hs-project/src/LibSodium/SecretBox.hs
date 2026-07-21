{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Secret-key authenticated encryption (@crypto_secretbox_easy@ /
-- @crypto_secretbox_open_easy@): one shared key, a per-message nonce, and a MAC
-- that authenticates the ciphertext.
--
-- 'encrypt' and 'open' are __pure__. Both are deterministic functions of their
-- arguments, so their specs are closed with
-- 'HsBindgen.HighLevel.toHighLevelPure' rather than
-- 'HsBindgen.HighLevel.toHighLevel' and no 'IO' reaches the type. What stays in 'IO'
-- is what genuinely draws on the outside world: 'newKey' and 'randomNonce' read the
-- CSPRNG.
--
-- Precondition: 'LibSodium.Init.sodiumInit' must have run before a pure result here
-- is /forced/, not merely built. Nothing enforces that.
--
-- 'encrypt' throws 'LibSodium.Error.SodiumError' on a setup failure, which reaches
-- the caller as an imprecise exception because the call site is pure; 'open' returns
-- 'Nothing' when authentication fails, since a forged ciphertext is expected input,
-- not an exception.
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

import HsBindgen.Runtime.PtrConst (PtrConst)

import HsBindgen.HighLevel (input2, output, toHighLevel, toHighLevelPure)
import HsBindgen.HighLevel.Auto (autoChecked, autoMaybe, autoResult)
import HsBindgen.HighLevel.Defaults (DefaultIn (..))
import HsBindgen.HighLevel.Marshaller (at)
import HsBindgen.HighLevel.Marshaller.Utils (byteStringOut, unsafeByteStringIn,
                                             unsafeByteStringLenIn)

import Generated.CryptoSecretbox (crypto_secretbox_KEYBYTES,
                                  crypto_secretbox_MACBYTES,
                                  crypto_secretbox_NONCEBYTES)
import Generated.CryptoSecretbox.Safe (crypto_secretbox_easy,
                                       crypto_secretbox_keygen,
                                       crypto_secretbox_open_easy)
import LibSodium.Error (checkStatus)
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

-- | A 32-byte secret key. The 'Key' constructor does not check the length; use
-- 'mkKey' (or 'newKey') for untrusted input, since C reads a wrong-length key out
-- of bounds.
newtype Key = Key { unKey :: ByteString }
  deriving stock (Eq, Show)

-- | A 24-byte nonce, unique per message under a given key. Like 'Key', prefer
-- 'mkNonce' for untrusted input.
newtype Nonce = Nonce { unNonce :: ByteString }
  deriving stock (Eq, Show)

-- A 'Key' and a 'Nonce' both reach C as @const unsigned char *@, unwrapped and
-- passed without copying.
instance DefaultIn Key (PtrConst CUChar -> lo) lo where
  defaultIn = at unKey unsafeByteStringIn

instance DefaultIn Nonce (PtrConst CUChar -> lo) lo where
  defaultIn = at unNonce unsafeByteStringIn

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

-- | A fresh random secret key (@crypto_secretbox_keygen@), drawn from libsodium's
-- CSPRNG.
newKey :: IO Key
newKey = toHighLevel crypto_secretbox_keygen
       $ output (Key <$> byteStringOut keyBytes) -- unsigned char k[32]
       $ autoResult

-- | A fresh random nonce.
randomNonce :: IO Nonce
randomNonce = Nonce <$> randomBytes nonceBytes

-- | Encrypt and authenticate @message@ under @key@ and @nonce@
-- (@crypto_secretbox_easy@). The ciphertext is @'macBytes' + length message@
-- bytes. Throws 'SodiumError' only on a setup failure (which @easy@ does not
-- signal in practice).
encrypt :: Key -> Nonce -> ByteString -> ByteString
encrypt key nonce message =
  toHighLevelPure crypto_secretbox_easy
    ( output (byteStringOut (macBytes + BS.length message)) -- unsigned char *c
    $ input2 unsafeByteStringLenIn                          -- m, mlen
    $ autoChecked (checkStatus "crypto_secretbox_easy")     -- n, k
    ) message nonce key

-- | Decrypt and verify @ciphertext@ (@crypto_secretbox_open_easy@). 'Nothing'
-- when the MAC does not match (a forged or corrupted ciphertext).
--
-- A rejected ciphertext is expected input rather than a failure, so this returns
-- 'Maybe' where 'encrypt' throws, which is what 'autoMaybe' is for: the status
-- decides between 'Just' and 'Nothing', and on 'Nothing' the plaintext buffer is
-- never read back, since a ciphertext that failed to authenticate put nothing in it.
--
-- Ciphertexts shorter than 'macBytes' cannot carry a MAC at all and are rejected
-- without calling C.
open :: Key -> Nonce -> ByteString -> Maybe ByteString
open key nonce ciphertext
  | BS.length ciphertext < macBytes = Nothing
  | otherwise =
      toHighLevelPure crypto_secretbox_open_easy
        ( output (byteStringOut (BS.length ciphertext - macBytes)) -- unsigned char *m
        $ input2 unsafeByteStringLenIn                             -- c, clen
        $ autoMaybe (== 0)                                         -- n, k
        ) ciphertext nonce key
