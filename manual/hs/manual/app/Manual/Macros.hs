module Manual.Macros (examples) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (ord)
import Foreign as F
import Foreign.C (CChar (..))
import Foreign.C qualified as FC
import System.IO

import Macro
import Macro.Unsafe
import Manual.Tools

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Macros"

    buffer <- mallocForeignPtrBytes 8
    (x :: Word32) <- withForeignPtr buffer $ \ptr -> do
      poke (plusPtr ptr (fromIntegral fIELD_OFFSET)) (1234 :: Word32)
      peek (pTR_TO_FIELD ptr)
    print x
    print (pTR_TO_FIELD (1 :: FC.CLong))

    year :: YEAR <- alloca $ \ptr -> do
      poke ptr $ Date (YEAR 2025) (MONTH 12) (DAY 25)
      getYear ptr
    print year

    subsection "Character and string literals"

    -- A character-literal macro becomes a CChar'.
    putStrLn $ "Character literal (letter): " <> show lETTER

    -- Special case: an escape sequence resolves to a single byte. '\a' is the
    -- ASCII BEL control character (byte 7).
    putStrLn $ "Character literal (bell):   " <> show bELL


    putStrLn $ "Use character literal in a function (greet in Chinese):"
    hFlush stdout
    greet cHINESE

    putStrLn $ "Use character literal in a function (greet in Japanese):"
    hFlush stdout
    greet jAPANESE

    putStrLn $ "Use character literal in a function (unknown language 'u'):"
    hFlush stdout
    greet (CChar $ fromIntegral $ ord 'u')

    putStrLn ""

    -- A string-literal macro becomes a pure 'BS.ByteString'. Since it is an
    -- ordinary value rather than an 'IO' action, we can render it in pure code.
    putStrLn $ "String literal (English):                         "
      <> (BS8.unpack gREETING)

    -- The byte-string holds exactly the bytes of the literal, with no implicit
    -- terminating NUL. 'BS.useAsCString' copies the bytes and appends the NUL
    -- that C string functions expect, then hands us a pointer to pass on.
    lenEng <- BS.useAsCString gREETING greeting_length
    putStrLn $ "String literal length (English, greeting_length): "
      <> show lenEng

    putStrLn ""

    -- Special case: a non-ASCII string literal is stored as its UTF-8 bytes,
    -- so the byte length (15) exceeds the number of characters (5). Writing the
    -- raw bytes to a UTF-8 terminal renders the original text.
    putStrLn $ "String literal (Japanese; Prelude.show): "
      <> show gREETING_JP
    putStr     "String literal (Japanese; BS.putStr):    "
      >> BS.putStr gREETING_JP
      >> putStrLn ""
    lenJap <- BS.useAsCString gREETING_JP greeting_length
    putStrLn $ "String literal length (Japanese, greeting_length):  "
      <> show lenJap
    putStrLn $ "String literal length (Japanese, BS.length):        "
      <> show (BS.length gREETING_JP)
