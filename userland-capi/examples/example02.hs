{-# LANGUAGE CApiFFI, TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import UserlandCApi
import Foreign.C.Types
import Foreign.C.String

foreign import capi "stdio.h printf" my_printf :: CString -> CInt -> IO ()

localname :: ()
localname = ()

$(userlandCApi 'localname $ do
    addInclude "<stdio.h>"
    w <- freshCName "printf"
    addC $ "void " ++ w ++ "(const char *fmt, int x) { printf(fmt, x); }"
    addDec $ foreignImport w "my_printf2" [t| CString -> CInt -> IO () |])

main :: IO ()
main = do
    withCString  "hello %d\n" $ \str -> my_printf str 42
    withCString  "hello %d\n" $ \str -> my_printf2 str 42
