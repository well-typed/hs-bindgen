-- | Ed25519 signatures, low-level.
--
-- The same program as "SignHigh" (identical output), written directly against
-- the generated bindings. Note the multipart path: the opaque @crypto_sign_state@
-- is allocated with 'alloca' and threaded by hand through init / update / final,
-- which the per-call high-level combinators cannot express.
module Main (main) where

import Control.Monad (forM_, when)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CUChar, CULLong)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Generated.Core.Safe (sodium_init)
import Generated.CryptoSign (Crypto_sign_state)
import Generated.CryptoSign.Safe (crypto_sign_detached,
                                  crypto_sign_final_create,
                                  crypto_sign_final_verify, crypto_sign_init,
                                  crypto_sign_keypair, crypto_sign_seed_keypair,
                                  crypto_sign_update,
                                  crypto_sign_verify_detached)
import Generated.Version.Safe (sodium_library_version_major,
                               sodium_library_version_minor)
import Numeric (showHex)

publicKeyBytes, secretKeyBytes, signatureBytes :: Int
publicKeyBytes = 32
secretKeyBytes = 64
signatureBytes = 64

main :: IO ()
main = do
  st <- sodium_init
  when (st < 0) $ fail "sodium_init failed"
  maj <- sodium_library_version_major
  mn  <- sodium_library_version_minor
  putStrLn $ "libsodium " ++ show (fromIntegral maj :: Int)
                          ++ "." ++ show (fromIntegral mn :: Int)

  let seed    = BS.pack [0 .. 31]
      chunks  = map BSC.pack ["The quick brown fox ", "jumps over ", "the lazy dog"]
      message = BS.concat chunks

  (pk, sk) <-
    allocaBytes publicKeyBytes $ \(pkPtr :: Ptr CUChar) ->
      allocaBytes secretKeyBytes $ \(skPtr :: Ptr CUChar) ->
      BSU.unsafeUseAsCStringLen seed $ \(sdPtr, _) -> do
        r <- crypto_sign_seed_keypair pkPtr skPtr (con sdPtr)
        when (r /= 0) $ fail "crypto_sign_seed_keypair failed"
        (,) <$> BS.packCStringLen (castPtr pkPtr, publicKeyBytes)
            <*> BS.packCStringLen (castPtr skPtr, secretKeyBytes)

  sig <-
    allocaBytes signatureBytes $ \(sigPtr :: Ptr CUChar) ->
      alloca $ \(siglenPtr :: Ptr CULLong) ->
      BSU.unsafeUseAsCStringLen message $ \(mPtr, mlen) ->
      BSU.unsafeUseAsCStringLen sk      $ \(skPtr, _) -> do
        r <- crypto_sign_detached sigPtr siglenPtr (con mPtr)
               (fromIntegral mlen) (con skPtr)
        when (r /= 0) $ fail "crypto_sign_detached failed"
        n <- peek siglenPtr
        BS.packCStringLen (castPtr sigPtr, fromIntegral n)

  putStrLn $ "seed:        " ++ toHex seed
  putStrLn $ "public key:  " ++ toHex pk
  putStrLn $ "secret key:  " ++ toHex sk
  putStrLn $ "signature:   " ++ toHex sig

  good <- verifyDetachedLow pk sig message
  putStrLn $ "verify:      " ++ if good then "ok" else "FAILED"
  bad <- verifyDetachedLow pk sig (flipFirst message)
  putStrLn $ "verify bad:  " ++ if bad then "ACCEPTED" else "rejected"

  msig <- signMultipartLow sk chunks
  putStrLn $ "multipart sig:    " ++ toHex msig
  mGood <- verifyMultipartLow pk msig chunks
  putStrLn $ "multipart verify: " ++ if mGood then "ok" else "FAILED"
  mBad <- verifyMultipartLow pk msig (tamper chunks)
  putStrLn $ "multipart bad:    " ++ if mBad then "ACCEPTED" else "rejected"

  (rpk, rsk) <-
    allocaBytes publicKeyBytes $ \(pkPtr :: Ptr CUChar) ->
      allocaBytes secretKeyBytes $ \(skPtr :: Ptr CUChar) -> do
        r <- crypto_sign_keypair pkPtr skPtr
        when (r /= 0) $ fail "crypto_sign_keypair failed"
        (,) <$> BS.packCStringLen (castPtr pkPtr, publicKeyBytes)
            <*> BS.packCStringLen (castPtr skPtr, secretKeyBytes)
  putStrLn $ "random keypair:   "
             ++ show (BS.length rpk) ++ " + " ++ show (BS.length rsk) ++ " bytes"

-- | A read-only view of a buffer pointer as libsodium's @const unsigned char *@.
con :: Ptr a -> PtrConst CUChar
con = PtrConst.unsafeFromPtr . castPtr

verifyDetachedLow :: ByteString -> ByteString -> ByteString -> IO Bool
verifyDetachedLow pk sig message =
  BSU.unsafeUseAsCStringLen sig     $ \(sigPtr, _) ->
    BSU.unsafeUseAsCStringLen message $ \(mPtr, mlen) ->
    BSU.unsafeUseAsCStringLen pk      $ \(pkPtr, _) -> do
      r <- crypto_sign_verify_detached (con sigPtr) (con mPtr)
             (fromIntegral mlen) (con pkPtr)
      pure (r == 0)

signMultipartLow :: ByteString -> [ByteString] -> IO ByteString
signMultipartLow sk chunks =
  alloca $ \(statePtr :: Ptr Crypto_sign_state) -> do
    initState statePtr chunks
    allocaBytes signatureBytes $ \(sigPtr :: Ptr CUChar) ->
      alloca $ \(siglenPtr :: Ptr CULLong) ->
      BSU.unsafeUseAsCStringLen sk $ \(skPtr, _) -> do
        r <- crypto_sign_final_create statePtr sigPtr siglenPtr (con skPtr)
        when (r /= 0) $ fail "crypto_sign_final_create failed"
        n <- peek siglenPtr
        BS.packCStringLen (castPtr sigPtr, fromIntegral n)

verifyMultipartLow :: ByteString -> ByteString -> [ByteString] -> IO Bool
verifyMultipartLow pk sig chunks =
  alloca $ \(statePtr :: Ptr Crypto_sign_state) -> do
    initState statePtr chunks
    BSU.unsafeUseAsCStringLen sig $ \(sigPtr, _) ->
      BSU.unsafeUseAsCStringLen pk $ \(pkPtr, _) -> do
        r <- crypto_sign_final_verify statePtr (con sigPtr) (con pkPtr)
        pure (r == 0)

-- | Initialise a signing state and feed every chunk through @crypto_sign_update@.
initState :: Ptr Crypto_sign_state -> [ByteString] -> IO ()
initState statePtr chunks = do
  ci <- crypto_sign_init statePtr
  when (ci /= 0) $ fail "crypto_sign_init failed"
  forM_ chunks $ \c ->
    BSU.unsafeUseAsCStringLen c $ \(p, len) -> do
      cu <- crypto_sign_update statePtr (con p) (fromIntegral len)
      when (cu /= 0) $ fail "crypto_sign_update failed"

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
