-- | Ed25519 signatures, high-level.
--
-- The same program as "SignLow" (identical output). The deterministic core is the
-- pure 'signDemo' (built on "LibSodium.Pure"); only the random 'keypair' stays in
-- 'IO'. Contrast the manual state threading in the low-level version.
module Main (main) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import LibSodium
import LibSodium.Pure qualified as Pure
import Numeric (showHex)

main :: IO ()
main = do
  sodiumInit
  (maj, mn) <- libraryVersion
  putStrLn $ "libsodium " ++ show maj ++ "." ++ show mn

  let seed   = Seed (BS.pack [0 .. 31])
      chunks = map BSC.pack ["The quick brown fox ", "jumps over ", "the lazy dog"]
      demo   = signDemo seed chunks
  putStrLn $ "seed:        " ++ toHex (unSeed seed)
  putStrLn $ "public key:  " ++ toHex (unPublicKey (publicKey demo))
  putStrLn $ "secret key:  " ++ toHex (unSecretKey (secretKey demo))
  putStrLn $ "signature:   " ++ toHex (unSignature (signature demo))
  putStrLn $ "verify:      " ++ if verifyGood demo then "ok" else "FAILED"
  putStrLn $ "verify bad:  " ++ if verifyBad demo then "ACCEPTED" else "rejected"
  putStrLn $ "multipart sig:    " ++ toHex (unSignature (multipartSig demo))
  putStrLn $ "multipart verify: " ++ if multipartGood demo then "ok" else "FAILED"
  putStrLn $ "multipart bad:    " ++ if multipartBad demo then "ACCEPTED" else "rejected"

  (rpk, rsk) <- keypair
  putStrLn $ "random keypair:   "
             ++ show (BS.length (unPublicKey rpk)) ++ " + "
             ++ show (BS.length (unSecretKey rsk)) ++ " bytes"

-- | The deterministic core: derive a keypair from @seed@, sign @chunks@ one-shot
-- and multipart, and verify each against the genuine and a tampered message.
-- Pure, so its type has no 'IO'.
data SignDemo = SignDemo
  { publicKey     :: PublicKey
  , secretKey     :: SecretKey
  , signature     :: Signature
  , verifyGood    :: Bool
  , verifyBad     :: Bool
  , multipartSig  :: Signature
  , multipartGood :: Bool
  , multipartBad  :: Bool
  }

signDemo :: Seed -> [ByteString] -> SignDemo
signDemo seed chunks = SignDemo
    { publicKey     = pk
    , secretKey     = sk
    , signature     = sig
    , verifyGood    = Pure.verifyDetached pk sig message
    , verifyBad     = Pure.verifyDetached pk sig (flipFirst message)
    , multipartSig  = msig
    , multipartGood = Pure.verifyMultipart pk msig chunks
    , multipartBad  = Pure.verifyMultipart pk msig (tamper chunks)
    }
  where
    (pk, sk) = Pure.seedKeypair seed
    sig      = Pure.signDetached sk message
    msig     = Pure.signMultipart sk chunks
    message  = BS.concat chunks

toHex :: ByteString -> String
toHex = concatMap byte . BS.unpack
  where byte w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

flipFirst :: ByteString -> ByteString
flipFirst bs = case BS.uncons bs of
  Just (w, rest) -> BS.cons (w `xor` 1) rest
  Nothing        -> bs

tamper :: [ByteString] -> [ByteString]
tamper []       = []
tamper (c : cs) = flipFirst c : cs
