{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Ed25519 public-key signatures: one-shot ('signDetached' \/ 'verifyDetached'),
-- streaming over message chunks ('signMultipart' \/ 'verifyMultipart'), and key
-- generation ('keypair' random, 'seedKeypair' deterministic from a seed).
--
-- Everything here except 'keypair' is __pure__. Ed25519 signing and verification are
-- deterministic, and so is deriving a keypair from a seed, so those five are
-- functions of their arguments and their types say so. 'keypair' draws from the
-- CSPRNG and stays in 'IO'.
--
-- All five close their spec with 'HsBindgen.HighLevel.toHighLevelPure', the multipart
-- pair included: the shared @crypto_sign_state@ enters the spec as a
-- 'HsBindgen.HighLevel.scratch' bracket, so those bindings are one spec too. See
-- 'signMultipart'.
--
-- Precondition: 'LibSodium.Init.sodiumInit' must have run before a pure result here
-- is /forced/, not merely built. Nothing enforces that. 'signDetached' and
-- 'signMultipart' throw 'LibSodium.Error.SodiumError' on a failure status, which
-- reaches the caller as an imprecise exception because the call site is pure.
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
import Foreign.C.Types (CInt, CUChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)

import HsBindgen.Runtime.PtrConst (PtrConst)

import HsBindgen.HighLevel (fixed, input, input2, output, resultIO, scratch,
                            throwOnNonZero, toHighLevel, toHighLevelPure)
import HsBindgen.HighLevel.Auto (autoChecked, autoInputs, autoWith)
import HsBindgen.HighLevel.Defaults (DefaultIn (..))
import HsBindgen.HighLevel.Marshaller (at, unmarshalOutPure)
import HsBindgen.HighLevel.Marshaller.Utils (byteStringOut, unsafeByteStringIn,
                                             unsafeByteStringLenIn)

import Generated.CryptoSign (Crypto_sign_state, crypto_sign_BYTES,
                             crypto_sign_PUBLICKEYBYTES,
                             crypto_sign_SECRETKEYBYTES, crypto_sign_SEEDBYTES)
import Generated.CryptoSign.Safe (crypto_sign_detached,
                                  crypto_sign_final_create,
                                  crypto_sign_final_verify, crypto_sign_init,
                                  crypto_sign_keypair, crypto_sign_seed_keypair,
                                  crypto_sign_update,
                                  crypto_sign_verify_detached)
import LibSodium.Error (checkStatus, sodiumError)

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

-- | Close a signing call: throw on a non-zero status, then trim the signature buffer
-- to the length the call reported. Ed25519 always fills it, so the trim is a formality,
-- but @crypto_sign_detached@ documents @siglen_p@ as the authority and this follows it.
--
-- Shared by the one-shot and multipart forms, which have the same result shape.
takeSignature :: String -> ByteString -> Int -> CInt -> IO Signature
takeSignature op sig siglen = (Signature (BS.take siglen sig) <$)
                            . checkStatus op

-- | A fresh random keypair (@crypto_sign_keypair@).
--
-- Each buffer is mapped into its key newtype as it is read, so a 'PublicKey' cannot
-- be passed where a 'SecretKey' is meant even though both are raw bytes.
keypair :: IO (PublicKey, SecretKey)
keypair = toHighLevel crypto_sign_keypair
        $ output (PublicKey <$> byteStringOut publicKeyBytes) -- unsigned char *pk
        $ output (SecretKey <$> byteStringOut secretKeyBytes) -- unsigned char *sk
        $ autoChecked (checkStatus "crypto_sign_keypair")

-- | A deterministic keypair from a 32-byte @seed@ (@crypto_sign_seed_keypair@).
--
-- Written point-free, so the wrapper type comes from the signature alone. That is
-- what 'autoChecked' needs to fill @seed@, and 'toHighLevelPure' passes it through:
-- the spec is checked against @Seed -> IO (PublicKey, SecretKey)@, the signature
-- below with the 'IO' put back.
seedKeypair :: Seed -> (PublicKey, SecretKey)
seedKeypair = toHighLevelPure crypto_sign_seed_keypair
            $ output (PublicKey <$> byteStringOut publicKeyBytes)  -- unsigned char *pk
            $ output (SecretKey <$> byteStringOut secretKeyBytes)  -- unsigned char *sk
            $ autoChecked (checkStatus "crypto_sign_seed_keypair") -- const unsigned char *seed

-- | Sign @message@ with @secretKey@, producing a detached 'Signature'
-- (@crypto_sign_detached@).
signDetached :: SecretKey -> ByteString -> Signature
signDetached secretKey message =
  toHighLevelPure crypto_sign_detached
    ( output (byteStringOut signatureBytes)  -- unsigned char *sig
    $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
    $ input2 unsafeByteStringLenIn           -- m, mlen
    $ input  defaultIn                       -- const unsigned char *sk
    $ resultIO (takeSignature "crypto_sign_detached")
    ) message secretKey

-- | Verify a detached signature (@crypto_sign_verify_detached@). 'False' when the
-- signature does not match, which is expected input rather than a failure, so this
-- does not throw.
verifyDetached :: PublicKey -> Signature -> ByteString -> Bool
verifyDetached publicKey signature message =
  toHighLevelPure crypto_sign_verify_detached (autoWith (== 0)) signature message publicKey

-- | Sign a message given as a sequence of @chunks@, using the multipart API
-- (@crypto_sign_init@ \/ @crypto_sign_update@ \/ @crypto_sign_final_create@).
--
-- 'withSignState' runs @init@ and every @update@, leaving a state @final_create@ can
-- draw the signature from. It is an ordinary bracket, so it goes in the spec as a
-- 'scratch' argument: 'scratch' takes any @forall r. (c -> IO r) -> IO r@, and the
-- combinators hold it open across the call the same way they hold an 'alloca' open.
-- The whole binding is therefore still one spec, and 'toHighLevelPure' closes it.
signMultipart :: SecretKey -> [ByteString] -> Signature
signMultipart secretKey chunks =
  toHighLevelPure crypto_sign_final_create
    ( scratch (withSignState chunks)         -- crypto_sign_state *state
    $ output (byteStringOut signatureBytes)  -- unsigned char *sig
    $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
    $ autoInputs                             -- const unsigned char *sk
    $ resultIO (takeSignature "crypto_sign_final_create")
    ) secretKey

-- | Verify a multipart signature (@crypto_sign_final_verify@). 'False' on a
-- mismatch, as for 'verifyDetached'. The state is a 'scratch' bracket, as at
-- 'signMultipart'.
verifyMultipart :: PublicKey -> Signature -> [ByteString] -> Bool
verifyMultipart publicKey signature chunks =
  toHighLevelPure crypto_sign_final_verify
    ( scratch (withSignState chunks) -- crypto_sign_state *state
    $ autoWith (== 0)                -- sig, pk; status -> Bool
    ) signature publicKey

-- | Allocate a @crypto_sign_state@, initialise it, fold every chunk through
-- @crypto_sign_update@, and hand the ready state to @use@.
--
-- Written as a bracket rather than as an @IO (Ptr ...)@ because that is the shape
-- 'scratch' consumes. @'withSignState' chunks@ is a
-- @forall r. ('Ptr' 'Crypto_sign_state' -> 'IO' r) -> 'IO' r@, so it drops into a spec
-- as a C argument the wrapper never exposes, and the combinators keep it open across
-- the call that follows. The @init@ and @update@ calls run when the bracket opens,
-- which is after the last wrapper argument arrives and before the final call, exactly
-- where a multipart API needs them.
withSignState :: [ByteString] -> (Ptr Crypto_sign_state -> IO r) -> IO r
withSignState chunks use =
  alloca $ \st -> do
    toHighLevel crypto_sign_init
      $ fixed st
      $ throwOnNonZero (sodiumError "crypto_sign_init")
    forM_ chunks $ \c ->
      toHighLevel crypto_sign_update
        ( fixed  st                    -- crypto_sign_state *state
        $ input2 unsafeByteStringLenIn -- m, mlen
        $ throwOnNonZero (sodiumError "crypto_sign_update")
        ) c
    use st
