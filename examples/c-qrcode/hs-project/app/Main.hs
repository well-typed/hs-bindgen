{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (guard)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Word (Word8)
import Foreign qualified as F
import Foreign.C qualified as F

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import QRCodeGenerator.Generated qualified as QR
import QRCodeGenerator.Generated.Safe qualified as QR

fromPtr
  :: forall a .
    F.Storable a
  => Int -> F.Ptr a -> IO (IncompleteArray a)
fromPtr len p = IA.peekArray len p'
  where
     p' :: F.Ptr (IncompleteArray a)
     p' = IA.toPtr p

-- static void printQr(const uint8_t qrcode[]) {
--  int size = qrcodegen_getSize(qrcode);
--  int border = 4;
--  for (int y = -border; y < size + border; y++) {
--    for (int x = -border; x < size + border; x++) {
--      fputs((qrcodegen_getModule(qrcode, x, y) ? "##" : "  "), stdout);
--    }
--    fputs("\n", stdout);
--  }
--  fputs("\n", stdout);
-- }
printQr :: IncompleteArray Word8 -> IO ()
printQr qrCode = do
  size <- IsA.withElemPtr qrCode $ \ptr -> QR.qrcodegen_getSize (PtrConst.unsafeFromPtr ptr)
  let border = 4
      range  = [-border .. size + border - 1]
  for_ range $ \y -> do
    for_ range $ \x -> do
      str <- bool "  " "██" . F.toBool <$>
        (IsA.withElemPtr qrCode $ \ptr -> QR.qrcodegen_getModule (PtrConst.unsafeFromPtr ptr) x y)
      putStr str
    putStr "\n"
  putStr "\n"

-- static void doBasicDemo(void) {
--  const char *text = "Hello, world!";               // User-supplied text
--  enum qrcodegen_Ecc errCorLvl = qrcodegen_Ecc_LOW; // Error correction level
--
--  // Make and print the QR Code symbol
--  uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
--  uint8_t tempBuffer[qrcodegen_BUFFER_LEN_MAX];
--  bool ok = qrcodegen_encodeText(text, tempBuffer, qrcode, errCorLvl,
--    qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
--  if (ok)
--    printQr(qrcode);
-- }
basicDemo :: IO ()
basicDemo = do
  F.withCAString "Hello, world!" $ \text ->
    F.allocaArray (fromIntegral QR.qrcodegen_BUFFER_LEN_MAX) $ \tempBuffer -> do
      F.allocaArray (fromIntegral QR.qrcodegen_BUFFER_LEN_MAX) $ \qrCode -> do
        b <- QR.qrcodegen_encodeText (PtrConst.unsafeFromPtr text) tempBuffer qrCode QR.Qrcodegen_Ecc_LOW
                                     QR.qrcodegen_VERSION_MIN QR.qrcodegen_VERSION_MAX
                                     QR.Qrcodegen_Mask_AUTO (F.fromBool True)
        qrCodeIA <- IA.peekArray (fromIntegral QR.qrcodegen_BUFFER_LEN_MAX) (IA.toPtr qrCode)
        guard (F.toBool b)
        printQr qrCodeIA

main :: IO ()
main = do
  basicDemo
