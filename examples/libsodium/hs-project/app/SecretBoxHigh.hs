-- | Secret-key authenticated encryption, high-level.
--
-- The same program as "SecretBoxLow" (identical output), written against the
-- high-level wrappers instead of the raw generated bindings. The deterministic
-- core is the pure 'secretboxDemo' (built on "LibSodium.Pure"); only key and
-- nonce generation stay in 'IO'.
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

  let key     = Key   (BS.pack [0 .. 31])
      nonce   = Nonce (BS.pack [0 .. 23])
      message = BSC.pack "The quick brown fox jumps over the lazy dog"
      (ciphertext, decrypted, tampered) = secretboxDemo key nonce message
  putStrLn $ "key:        " ++ toHex (unKey key)
  putStrLn $ "nonce:      " ++ toHex (unNonce nonce)
  putStrLn $ "message:    " ++ show (BSC.unpack message)
  putStrLn $ "ciphertext: " ++ toHex ciphertext
  putStrLn $ "decrypted:  " ++ maybe "<rejected>" (show . BSC.unpack) decrypted
  putStrLn $ "tampered:   " ++ maybe "rejected" (const "ACCEPTED") tampered

  k <- newKey
  n <- randomNonce
  putStrLn $ "keygen len: " ++ show (BS.length (unKey k))
  putStrLn $ "nonce len:  " ++ show (BS.length (unNonce n))

-- | The deterministic core: encrypt @message@, then open both the genuine
-- ciphertext and a tampered copy. Pure, so its type has no 'IO'.
secretboxDemo :: Key -> Nonce -> ByteString
              -> (ByteString, Maybe ByteString, Maybe ByteString)
secretboxDemo key nonce message =
    ( ciphertext
    , Pure.open key nonce ciphertext
    , Pure.open key nonce (flipFirst ciphertext)
    )
  where
    ciphertext = Pure.encrypt key nonce message

toHex :: ByteString -> String
toHex = concatMap byte . BS.unpack
  where byte w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

flipFirst :: ByteString -> ByteString
flipFirst bs = case BS.uncons bs of
  Just (w, rest) -> BS.cons (w `xor` 1) rest
  Nothing        -> bs
