{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ed25519 public-key signatures.
--
-- Three combinator shapes appear here that secretbox does not have:
--
--   * 'keypair' \/ 'seedKeypair' write /two/ output buffers (public and secret
--     key), so two 'output's stack before the closer.
--   * 'signDetached' has an out-length pointer (@siglen_p@): a scalar 'output'
--     that /does/ have a default marshaller ('unmarshalOutPure'), unlike the
--     byte-buffer output.
--   * the multipart path ('signMultipart' \/ 'verifyMultipart') threads an opaque
--     @crypto_sign_state@ through @init@\/@update@\/@final@. The combinators are
--     per-call, so the sequencing is plain hand-written Haskell (the analogue of
--     libgit2's stateful iterator).
module LibSodium.Sign
  ( -- * Types
    PublicKey (..)
  , SecretKey (..)
  , Signature (..)
  , Seed (..)
  , publicKeyBytes
  , secretKeyBytes
  , signatureBytes
  , seedBytes
    -- * Key generation
  , keypair
  , seedKeypair
    -- * One-shot signatures
  , signDetached
  , verifyDetached
    -- * Multipart (streaming) signatures
  , signMultipart
  , verifyMultipart
  ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Types (CUChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.HighLevel (input, input2, output, resultPure,
                                    throwOnNonZero, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (DefaultIn (..))
import HsBindgen.Runtime.HighLevel.Marshaller (at, unmarshalOutPure)
import HsBindgen.Runtime.PtrConst (PtrConst)

import Generated.CryptoSign (Crypto_sign_state, crypto_sign_BYTES,
                             crypto_sign_PUBLICKEYBYTES,
                             crypto_sign_SECRETKEYBYTES, crypto_sign_SEEDBYTES)
import Generated.CryptoSign.Safe (crypto_sign_detached,
                                  crypto_sign_final_create,
                                  crypto_sign_final_verify, crypto_sign_init,
                                  crypto_sign_keypair, crypto_sign_seed_keypair,
                                  crypto_sign_update,
                                  crypto_sign_verify_detached)
import LibSodium.Error (sodiumError)
import LibSodium.Marshal (byteStringOut, bytesConstIn, bytesLenConstIn, fixed)

-- | Public-key size in bytes (32).
publicKeyBytes :: Int
publicKeyBytes = fromIntegral crypto_sign_PUBLICKEYBYTES

-- | Secret-key size in bytes (64).
secretKeyBytes :: Int
secretKeyBytes = fromIntegral crypto_sign_SECRETKEYBYTES

-- | Signature size in bytes (64).
signatureBytes :: Int
signatureBytes = fromIntegral crypto_sign_BYTES

-- | Seed size in bytes (32), for deterministic key generation.
seedBytes :: Int
seedBytes = fromIntegral crypto_sign_SEEDBYTES

-- | A 32-byte Ed25519 public key.
newtype PublicKey = PublicKey { unPublicKey :: ByteString }
  deriving stock (Eq, Show)

-- | A 64-byte Ed25519 secret key.
newtype SecretKey = SecretKey { unSecretKey :: ByteString }
  deriving stock (Eq, Show)

-- | A 64-byte detached signature.
newtype Signature = Signature { unSignature :: ByteString }
  deriving stock (Eq, Show)

-- | A 32-byte seed. @'seedKeypair' seed@ is deterministic in @seed@.
newtype Seed = Seed { unSeed :: ByteString }
  deriving stock (Eq, Show)

instance DefaultIn PublicKey where
  type DefInArrow PublicKey lo = PtrConst CUChar -> lo
  defaultIn = at unPublicKey bytesConstIn

instance DefaultIn SecretKey where
  type DefInArrow SecretKey lo = PtrConst CUChar -> lo
  defaultIn = at unSecretKey bytesConstIn

instance DefaultIn Signature where
  type DefInArrow Signature lo = PtrConst CUChar -> lo
  defaultIn = at unSignature bytesConstIn

instance DefaultIn Seed where
  type DefInArrow Seed lo = PtrConst CUChar -> lo
  defaultIn = at unSeed bytesConstIn

-- | A fresh random keypair (@crypto_sign_keypair@). Two output buffers, one
-- status.
keypair :: IO (PublicKey, SecretKey)
keypair =
  drop3 <$> toHighLevel
    ( output (byteStringOut publicKeyBytes)  -- pk
    $ output (byteStringOut secretKeyBytes)  -- sk
    $ throwOnNonZero (sodiumError "crypto_sign_keypair")
    ) crypto_sign_keypair
  where
    drop3 (pk, sk, ()) = (PublicKey pk, SecretKey sk)

-- | A deterministic keypair from a 32-byte @seed@ (@crypto_sign_seed_keypair@).
seedKeypair :: Seed -> IO (PublicKey, SecretKey)
seedKeypair seed =
  drop3 <$> toHighLevel
    ( output (byteStringOut publicKeyBytes)  -- pk
    $ output (byteStringOut secretKeyBytes)  -- sk
    $ input  defaultIn                       -- seed
    $ throwOnNonZero (sodiumError "crypto_sign_seed_keypair")
    ) crypto_sign_seed_keypair seed
  where
    drop3 (pk, sk, ()) = (PublicKey pk, SecretKey sk)

-- | Sign @message@ with @secretKey@, producing a detached 'Signature'
-- (@crypto_sign_detached@). The @siglen_p@ out-length is peeked with the scalar
-- 'unmarshalOutPure' default and used to trim the signature buffer (always
-- 'signatureBytes' for Ed25519).
signDetached :: SecretKey -> ByteString -> IO Signature
signDetached secretKey message =
  mkSig <$> toHighLevel
    ( output (byteStringOut signatureBytes)   -- sig
    $ output (unmarshalOutPure fromIntegral)  -- siglen_p -> Int
    $ input2 bytesLenConstIn                  -- m, mlen
    $ input  defaultIn                        -- sk
    $ throwOnNonZero (sodiumError "crypto_sign_detached")
    ) crypto_sign_detached message secretKey
  where
    mkSig (sig, siglen, ()) = Signature (BS.take siglen sig)

-- | Verify a detached signature (@crypto_sign_verify_detached@). 'False' when the
-- signature does not match (expected control flow, so no exception). All inputs,
-- a status mapped to 'Bool': @auto@ could fill the inputs, but the @0 -> True@
-- result mapping keeps the closer explicit.
verifyDetached :: PublicKey -> Signature -> ByteString -> IO Bool
verifyDetached publicKey signature message =
  classify <$> toHighLevel
    ( input  defaultIn        -- sig
    $ input2 bytesLenConstIn  -- m, mlen
    $ input  defaultIn        -- pk
    $ resultPure id           -- raw status
    ) crypto_sign_verify_detached signature message publicKey
  where
    classify status = status == 0

-- | Sign a message given as a sequence of @chunks@, using the multipart API
-- (@crypto_sign_init@ \/ @crypto_sign_update@ \/ @crypto_sign_final_create@).
--
-- The opaque @crypto_sign_state@ is allocated once with 'alloca' (it derives
-- 'Foreign.Storable.Storable') and passed to each call with 'fixed'. Everything
-- else is combinators: 'initState' folds the chunks in, and @final_create@ writes
-- the signature and its length through 'output's. The one thing the per-call
-- combinators cannot express is the shared state, which is exactly the single
-- 'alloca' plus a 'fixed' per call.
signMultipart :: SecretKey -> [ByteString] -> IO Signature
signMultipart secretKey chunks =
  alloca $ \st -> do
    initState st chunks
    mkSig <$> toHighLevel
      ( fixed  st                               -- crypto_sign_state *state
      $ output (byteStringOut signatureBytes)   -- sig
      $ output (unmarshalOutPure fromIntegral)  -- siglen_p -> Int
      $ input  defaultIn                        -- sk
      $ throwOnNonZero (sodiumError "crypto_sign_final_create")
      ) crypto_sign_final_create secretKey
  where
    mkSig (sig, siglen, ()) = Signature (BS.take siglen sig)

-- | Verify a multipart signature (@crypto_sign_final_verify@). 'False' on a
-- mismatch.
verifyMultipart :: PublicKey -> Signature -> [ByteString] -> IO Bool
verifyMultipart publicKey signature chunks =
  alloca $ \st -> do
    initState st chunks
    classify <$> toHighLevel
      ( fixed st          -- crypto_sign_state *state
      $ input defaultIn   -- sig
      $ input defaultIn   -- pk
      $ resultPure id     -- raw status
      ) crypto_sign_final_verify signature publicKey
  where
    classify status = status == 0

-- | Initialise a signing state and fold every chunk through @crypto_sign_update@,
-- each call reusing the pre-allocated state via 'fixed'. Shared by
-- 'signMultipart' and 'verifyMultipart'.
initState :: Ptr Crypto_sign_state -> [ByteString] -> IO ()
initState st chunks = do
  toHighLevel (fixed st $ throwOnNonZero (sodiumError "crypto_sign_init"))
    crypto_sign_init
  forM_ chunks $ \c ->
    toHighLevel
      ( fixed  st                -- state
      $ input2 bytesLenConstIn   -- m, mlen
      $ throwOnNonZero (sodiumError "crypto_sign_update")
      ) crypto_sign_update c
