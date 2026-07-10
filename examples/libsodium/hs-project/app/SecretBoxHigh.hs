-- | Secret-key authenticated encryption, high-level.
--
-- The same program as "SecretBoxLow" (identical output), written against the
-- high-level "LibSodium.SecretBox" wrappers instead of the raw generated
-- bindings. No @alloca@, no pointer casts, no manual status checks.
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

  let key     = Key   (BS.pack [0 .. 31])
      nonce   = Nonce (BS.pack [0 .. 23])
      message = BSC.pack "The quick brown fox jumps over the lazy dog"

  ciphertext <- encrypt key nonce message
  putStrLn $ "key:        " ++ toHex (unKey key)
  putStrLn $ "nonce:      " ++ toHex (unNonce nonce)
  putStrLn $ "message:    " ++ show (BSC.unpack message)
  putStrLn $ "ciphertext: " ++ toHex ciphertext

  decrypted <- open key nonce ciphertext
  putStrLn $ "decrypted:  " ++ maybe "<rejected>" (show . BSC.unpack) decrypted

  tampered <- open key nonce (flipFirst ciphertext)
  putStrLn $ "tampered:   " ++ maybe "rejected" (const "ACCEPTED") tampered

  k <- newKey
  n <- randomNonce
  putStrLn $ "keygen len: " ++ show (BS.length (unKey k))
  putStrLn $ "nonce len:  " ++ show (BS.length (unNonce n))

toHex :: ByteString -> String
toHex = concatMap byte . BS.unpack
  where byte w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

flipFirst :: ByteString -> ByteString
flipFirst bs = case BS.uncons bs of
  Just (w, rest) -> BS.cons (w `xor` 1) rest
  Nothing        -> bs
