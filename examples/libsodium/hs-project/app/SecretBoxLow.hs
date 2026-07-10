-- | Secret-key authenticated encryption, low-level.
--
-- The same program as "SecretBoxHigh" (identical output), written directly
-- against the generated bindings: manual @allocaBytes@ for every output buffer,
-- @unsafeUseAsCStringLen@ plus a pointer cast for every input, and hand-checked
-- status codes.
module Main (main) where

import Control.Monad (when)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CUChar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Core.Safe (sodium_init)
import Generated.CryptoSecretbox.Safe (crypto_secretbox_easy,
                                       crypto_secretbox_keygen,
                                       crypto_secretbox_open_easy)
import Generated.Randombytes.Safe (randombytes_buf)
import Generated.Version.Safe (sodium_library_version_major,
                               sodium_library_version_minor)
import Numeric (showHex)

keyBytes, nonceBytes, macBytes :: Int
keyBytes   = 32
nonceBytes = 24
macBytes   = 16

main :: IO ()
main = do
  st <- sodium_init
  when (st < 0) $ fail "sodium_init failed"
  maj <- sodium_library_version_major
  mn  <- sodium_library_version_minor
  putStrLn $ "libsodium " ++ show (fromIntegral maj :: Int)
                          ++ "." ++ show (fromIntegral mn :: Int)

  let key     = BS.pack [0 .. 31]
      nonce   = BS.pack [0 .. 23]
      message = BSC.pack "The quick brown fox jumps over the lazy dog"
      clen    = BS.length message + macBytes

  ciphertext <-
    allocaBytes clen $ \(cPtr :: Ptr CUChar) ->
      BSU.unsafeUseAsCStringLen message $ \(mPtr, mlen) ->
      BSU.unsafeUseAsCStringLen nonce   $ \(nPtr, _) ->
      BSU.unsafeUseAsCStringLen key     $ \(kPtr, _) -> do
        r <- crypto_secretbox_easy cPtr (con mPtr) (fromIntegral mlen)
               (con nPtr) (con kPtr)
        when (r /= 0) $ fail "crypto_secretbox_easy failed"
        BS.packCStringLen (castPtr cPtr, clen)

  putStrLn $ "key:        " ++ toHex key
  putStrLn $ "nonce:      " ++ toHex nonce
  putStrLn $ "message:    " ++ show (BSC.unpack message)
  putStrLn $ "ciphertext: " ++ toHex ciphertext

  decrypted <- secretboxOpen key nonce ciphertext
  putStrLn $ "decrypted:  " ++ maybe "<rejected>" (show . BSC.unpack) decrypted

  tampered <- secretboxOpen key nonce (flipFirst ciphertext)
  putStrLn $ "tampered:   " ++ maybe "rejected" (const "ACCEPTED") tampered

  kLen <- allocaBytes keyBytes $ \(p :: Ptr CUChar) -> do
    crypto_secretbox_keygen (castPtr p)
    BS.length <$> BS.packCStringLen (castPtr p, keyBytes)
  nLen <- allocaBytes nonceBytes $ \(p :: Ptr CUChar) -> do
    randombytes_buf (castPtr p) (fromIntegral nonceBytes)
    BS.length <$> BS.packCStringLen (castPtr p, nonceBytes)
  putStrLn $ "keygen len: " ++ show kLen
  putStrLn $ "nonce len:  " ++ show nLen

-- | A read-only view of a buffer pointer as libsodium's @const unsigned char *@.
con :: Ptr a -> PtrConst CUChar
con = PtrConst.unsafeFromPtr . castPtr

secretboxOpen :: ByteString -> ByteString -> ByteString -> IO (Maybe ByteString)
secretboxOpen key nonce ciphertext
  | BS.length ciphertext < macBytes = pure Nothing
  | otherwise =
      allocaBytes mlen $ \(mPtr :: Ptr CUChar) ->
        BSU.unsafeUseAsCStringLen ciphertext $ \(cPtr, _) ->
        BSU.unsafeUseAsCStringLen nonce      $ \(nPtr, _) ->
        BSU.unsafeUseAsCStringLen key        $ \(kPtr, _) -> do
          r <- crypto_secretbox_open_easy mPtr (con cPtr)
                 (fromIntegral (BS.length ciphertext)) (con nPtr) (con kPtr)
          if r == 0
            then Just <$> BS.packCStringLen (castPtr mPtr, mlen)
            else pure Nothing
  where
    mlen = BS.length ciphertext - macBytes

toHex :: ByteString -> String
toHex = concatMap byte . BS.unpack
  where byte w = let s = showHex w "" in replicate (2 - length s) '0' ++ s

flipFirst :: ByteString -> ByteString
flipFirst bs = case BS.uncons bs of
  Just (w, rest) -> BS.cons (w `xor` 1) rest
  Nothing        -> bs
