{-# LANGUAGE CApiFFI         #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foreign.C.String           (CString, withCString)
import Foreign.C.Types            (CInt (..))
import Language.Haskell.TH.Syntax (ForeignSrcLang (LangC), addForeignSource)

-- we can use CApiFFI to import printf with some fixed signature
foreign import capi "stdio.h printf" my_printf :: CString -> CInt -> IO ()

-- or we can create wrapper ourselves, all from Haskell source:
$(do
    addForeignSource LangC $ unlines
        [ "#include <stdio.h>"
        , "void printf_wrapper(const char *fmt, int x) { printf(fmt, x); }"
        ]

    [d|
        foreign import ccall "printf_wrapper" my_printf2 :: CString -> CInt -> IO ()
      |])

main :: IO ()
main = do
    withCString "hello %d\n" $ \str -> my_printf str 42
    withCString "hello %d\n" $ \str -> my_printf2 str 42
