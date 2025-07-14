module Main where

import qualified QRCodeGenerator.Generated as QR
import qualified Foreign as F
import qualified Foreign.C as F
import Data.Word (Word8)
import Control.Monad (guard)
import Data.Foldable (for_)
import Data.Bool (bool)


-- static void printQr(const uint8_t qrcode[]) {
-- 	int size = qrcodegen_getSize(qrcode);
-- 	int border = 4;
-- 	for (int y = -border; y < size + border; y++) {
-- 		for (int x = -border; x < size + border; x++) {
-- 			fputs((qrcodegen_getModule(qrcode, x, y) ? "##" : "  "), stdout);
-- 		}
-- 		fputs("\n", stdout);
-- 	}
-- 	fputs("\n", stdout);
-- }
printQr :: F.Ptr Word8 -> IO ()
printQr qrCode = do
  size <- QR.qrcodegen_getSize qrCode
  let border = 4
      range  = [-border .. size + border - 1]
  for_ range $ \y -> do
    for_ range $ \x -> do
      str <- bool "  " "██" . F.toBool <$> QR.qrcodegen_getModule qrCode x y
      putStr str
    putStr "\n"
  putStr "\n"

-- static void doBasicDemo(void) {
-- 	const char *text = "Hello, world!";                // User-supplied text
-- 	enum qrcodegen_Ecc errCorLvl = qrcodegen_Ecc_LOW;  // Error correction level
--
-- 	// Make and print the QR Code symbol
-- 	uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
-- 	uint8_t tempBuffer[qrcodegen_BUFFER_LEN_MAX];
-- 	bool ok = qrcodegen_encodeText(text, tempBuffer, qrcode, errCorLvl,
-- 		qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
-- 	if (ok)
-- 		printQr(qrcode);
-- }
basicDemo :: IO ()
basicDemo = do
  F.withCAString "Hello, world!" $ \text ->
    F.allocaArray (fromIntegral QR.qrcodegen_BUFFER_LEN_MAX) $ \tempBuffer ->
      F.allocaArray (fromIntegral QR.qrcodegen_BUFFER_LEN_MAX) $ \qrCode -> do
        b <- QR.qrcodegen_encodeText text tempBuffer qrCode QR.Qrcodegen_Ecc_LOW
                                    QR.qrcodegen_VERSION_MIN QR.qrcodegen_VERSION_MAX
                                    QR.Qrcodegen_Mask_AUTO (F.fromBool True)
        guard (F.toBool b)
        printQr qrCode

main :: IO ()
main = do
  basicDemo
