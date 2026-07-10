-- | Ed25519 signatures, high-level.
--
-- The same program as "SignLow" (identical output), written against
-- "LibSodium.Sign". Keypair generation, detached sign/verify, and the multipart
-- streaming path are each a single call; contrast the manual state threading in
-- the low-level version.
module Main (main) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

import LibSodium
import Numeric (showHex)

main :: IO ()
main = withSodium $ do
  (maj, mn) <- libraryVersion
  putStrLn $ "libsodium " ++ show maj ++ "." ++ show mn

  let seed    = Seed (BS.pack [0 .. 31])
      chunks  = map BSC.pack ["The quick brown fox ", "jumps over ", "the lazy dog"]
      message = BS.concat chunks

  (pk, sk) <- seedKeypair seed
  sig      <- signDetached sk message
  putStrLn $ "seed:        " ++ toHex (unSeed seed)
  putStrLn $ "public key:  " ++ toHex (unPublicKey pk)
  putStrLn $ "secret key:  " ++ toHex (unSecretKey sk)
  putStrLn $ "signature:   " ++ toHex (unSignature sig)

  good <- verifyDetached pk sig message
  putStrLn $ "verify:      " ++ if good then "ok" else "FAILED"
  bad <- verifyDetached pk sig (flipFirst message)
  putStrLn $ "verify bad:  " ++ if bad then "ACCEPTED" else "rejected"

  msig <- signMultipart sk chunks
  putStrLn $ "multipart sig:    " ++ toHex (unSignature msig)
  mGood <- verifyMultipart pk msig chunks
  putStrLn $ "multipart verify: " ++ if mGood then "ok" else "FAILED"
  mBad <- verifyMultipart pk msig (tamper chunks)
  putStrLn $ "multipart bad:    " ++ if mBad then "ACCEPTED" else "rejected"

  (rpk, rsk) <- keypair
  putStrLn $ "random keypair:   "
             ++ show (BS.length (unPublicKey rpk)) ++ " + "
             ++ show (BS.length (unSecretKey rsk)) ++ " bytes"

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
