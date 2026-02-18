{-# LANGUAGE TemplateHaskell #-}
module Lib (myStrlen) where

import Foreign.C
import Language.Haskell.TH.Syntax (addForeignSource, ForeignSrcLang(..))

-- Use TH to add C source code that calls strlen.
-- GHC compiles this C code with 'gcc -c' (no link step), producing a .o
-- file where strlen is STT_NOTYPE. On ARM32, the runtime linker fails to
-- detect that strlen is a Thumb function, generating BL instead of BLX.
$(do
    addForeignSource LangC $ unlines
      [ "#include <string.h>"
      , "int my_strlen(const char *s) {"
      , "    return (int)strlen(s);"
      , "}"
      ]
    return []
 )

foreign import ccall "my_strlen" myStrlen :: CString -> IO CInt
