{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Ed25519 public-key signatures: one-shot ('signDetached' \/ 'verifyDetached'),
-- streaming over message chunks ('signMultipart' \/ 'verifyMultipart'), and key
-- generation ('keypair' random, 'seedKeypair' deterministic from a seed).
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

import HsBindgen.Runtime.HighLevel (dropTrailingUnit, fixed, input, input2,
                                    output, resultPure, throwOnNonZero,
                                    toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (DefaultIn (..))
import HsBindgen.Runtime.HighLevel.Marshaller (at, unmarshalOutPure)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (byteStringOut,
                                                     constByteStringLenIn,
                                                     unsafeByteStringIn)
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

instance DefaultIn PublicKey (PtrConst CUChar -> lo) lo where
  defaultIn = at unPublicKey unsafeByteStringIn

instance DefaultIn SecretKey (PtrConst CUChar -> lo) lo where
  defaultIn = at unSecretKey unsafeByteStringIn

instance DefaultIn Signature (PtrConst CUChar -> lo) lo where
  defaultIn = at unSignature unsafeByteStringIn

instance DefaultIn Seed (PtrConst CUChar -> lo) lo where
  defaultIn = at unSeed unsafeByteStringIn

-- | Trim the signature buffer to its out-length. It stays a small post-processor
-- because the trim needs both outputs (the buffer and its length), which no single
-- 'output' can join; 'dropTrailingUnit' has already removed the status @()@.
mkSignature :: (ByteString, Int) -> Signature
mkSignature (sig, siglen) = Signature (BS.take siglen sig)

-- | A fresh random keypair (@crypto_sign_keypair@). Two output buffers, one
-- status; each buffer is wrapped into its key newtype by mapping the 'Unmarshaller',
-- and 'dropTrailingUnit' removes the status @()@.
keypair :: IO (PublicKey, SecretKey)
keypair =
  toHighLevel
    ( dropTrailingUnit
    $ output (PublicKey <$> byteStringOut publicKeyBytes)  -- pk
    $ output (SecretKey <$> byteStringOut secretKeyBytes)  -- sk
    $ throwOnNonZero (sodiumError "crypto_sign_keypair")
    ) crypto_sign_keypair

-- | A deterministic keypair from a 32-byte @seed@ (@crypto_sign_seed_keypair@).
seedKeypair :: Seed -> IO (PublicKey, SecretKey)
seedKeypair seed =
  toHighLevel
    ( dropTrailingUnit
    $ output (PublicKey <$> byteStringOut publicKeyBytes)  -- pk
    $ output (SecretKey <$> byteStringOut secretKeyBytes)  -- sk
    $ input  defaultIn                                     -- seed
    $ throwOnNonZero (sodiumError "crypto_sign_seed_keypair")
    ) crypto_sign_seed_keypair seed

-- | Sign @message@ with @secretKey@, producing a detached 'Signature'
-- (@crypto_sign_detached@). The @siglen_p@ out-length is peeked with the scalar
-- 'unmarshalOutPure' default and used to trim the signature buffer (always
-- 'signatureBytes' for Ed25519).
signDetached :: SecretKey -> ByteString -> IO Signature
signDetached secretKey message =
  mkSignature <$> toHighLevel
    ( dropTrailingUnit
    $ output (byteStringOut signatureBytes)   -- sig
    $ output (unmarshalOutPure fromIntegral)  -- siglen_p -> Int
    $ input2 constByteStringLenIn             -- m, mlen
    $ input  defaultIn                        -- sk
    $ throwOnNonZero (sodiumError "crypto_sign_detached")
    ) crypto_sign_detached message secretKey

-- | Verify a detached signature (@crypto_sign_verify_detached@). 'False' when the
-- signature does not match (expected control flow, so no exception). The @status == 0@
-- mapping to 'Bool' sits inside the spec as @resultPure (== 0)@. @auto@ cannot fill
-- these inputs: 'Signature'\/'PublicKey' share the same @const unsigned char *@ C
-- shape, so it cannot pick a 'DefaultIn' instance from the C type alone.
verifyDetached :: PublicKey -> Signature -> ByteString -> IO Bool
verifyDetached publicKey signature message =
  toHighLevel
    ( input  defaultIn             -- sig
    $ input2 constByteStringLenIn  -- m, mlen
    $ input  defaultIn             -- pk
    $ resultPure (== 0)            -- status -> Bool
    ) crypto_sign_verify_detached signature message publicKey

-- | Sign a message given as a sequence of @chunks@, using the multipart API
-- (@crypto_sign_init@ \/ @crypto_sign_update@ \/ @crypto_sign_final_create@).
--
-- 'withSignState' holds the running @crypto_sign_state@ (see its note on why the
-- state cannot be a 'scratch'); @final_create@ then writes the signature and its
-- length through 'output's, and 'dropTrailingUnit' removes the status @()@.
signMultipart :: SecretKey -> [ByteString] -> IO Signature
signMultipart secretKey chunks =
  withSignState chunks $ \st ->
    mkSignature <$> toHighLevel
      ( dropTrailingUnit
      $ fixed  st                               -- crypto_sign_state *state
      $ output (byteStringOut signatureBytes)   -- sig
      $ output (unmarshalOutPure fromIntegral)  -- siglen_p -> Int
      $ input  defaultIn                        -- sk
      $ throwOnNonZero (sodiumError "crypto_sign_final_create")
      ) crypto_sign_final_create secretKey

-- | Verify a multipart signature (@crypto_sign_final_verify@). 'False' on a
-- mismatch; the @status == 0@ mapping to 'Bool' sits inside the spec.
verifyMultipart :: PublicKey -> Signature -> [ByteString] -> IO Bool
verifyMultipart publicKey signature chunks =
  withSignState chunks $ \st ->
    toHighLevel
      ( fixed st          -- crypto_sign_state *state
      $ input defaultIn   -- sig
      $ input defaultIn   -- pk
      $ resultPure (== 0)
      ) crypto_sign_final_verify signature publicKey

-- | Allocate a @crypto_sign_state@, initialise it, fold every chunk through
-- @crypto_sign_update@, and hand the ready state to @use@.
--
-- The state is one 'alloca' shared across the @init@ \/ @update@ \/ final calls. It
-- cannot be a 'scratch' or 'fixed' argument of a single 'toHighLevel' spec: those
-- live only for the one call, but the streaming state must outlive every call, which
-- is inherent to any multipart (incremental) API. This bracket is why a
-- @Ptr Crypto_sign_state@ is threaded between the calls.
withSignState :: [ByteString] -> (Ptr Crypto_sign_state -> IO r) -> IO r
withSignState chunks use =
  alloca $ \st -> do
    toHighLevel (fixed st $ throwOnNonZero (sodiumError "crypto_sign_init"))
      crypto_sign_init
    forM_ chunks $ \c ->
      toHighLevel
        ( fixed  st                     -- state
        $ input2 constByteStringLenIn   -- m, mlen
        $ throwOnNonZero (sodiumError "crypto_sign_update")
        ) crypto_sign_update c
    use st
