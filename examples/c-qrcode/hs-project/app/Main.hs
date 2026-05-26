{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (guard)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.Word (Word8)

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)

import HighLevel qualified as HL
import QRCodeGenerator.Generated qualified as QR

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
  size <- HL.getSize qrCode
  let border = 4
      range  = [-border .. size + border - 1]
  for_ range $ \y -> do
    for_ range $ \x -> do
      isOn <- HL.getModule qrCode x y
      putStr (bool "  " "██" isOn)
    putStr "\n"
  putStr "\n"

-- static void doBasicDemo(void) {
--  const char *text = "Hello, world!";
--  enum qrcodegen_Ecc errCorLvl = qrcodegen_Ecc_LOW;
--  uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
--  uint8_t tempBuffer[qrcodegen_BUFFER_LEN_MAX];
--  bool ok = qrcodegen_encodeText(text, tempBuffer, qrcode, errCorLvl,
--    qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
--  if (ok) printQr(qrcode);
-- }
basicDemo :: IO ()
basicDemo = do
  ((), (qrCode, ok)) <- HL.encodeText
                          "Hello, world!"
                          QR.Qrcodegen_Ecc_LOW
                          (fromIntegral QR.qrcodegen_VERSION_MIN)
                          (fromIntegral QR.qrcodegen_VERSION_MAX)
                          QR.Qrcodegen_Mask_AUTO
                          True
  guard ok
  printQr qrCode

main :: IO ()
main = basicDemo
