module Example
    ( Example.iNCR
    , Example.aDD
    , Example.iD
    , Example.cONST
    , Example.cONST_3
    , Example.cMP
    , Example.fUN1
    , Example.fUN2
    , Example.g
    , Example.g_3
    , Example.dIV1
    , Example.dIV2
    , Example.sWAP32
    , Example.aV_VERSION_INT
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro INCR@

    __defined at:__ @macros\/macro_functions.h 1:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iNCR :: Macro.Raw BG.Text
iNCR =
  Macro.functionLike "INCR" ["x"] ["x", "+", "1"]

{-| __C declaration:__ @macro ADD@

    __defined at:__ @macros\/macro_functions.h 2:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aDD :: Macro.Raw BG.Text
aDD =
  Macro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]

{-| __C declaration:__ @macro ID@

    __defined at:__ @macros\/macro_functions.h 4:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iD :: Macro.Raw BG.Text
iD = Macro.functionLike "ID" ["X"] ["X"]

{-| __C declaration:__ @macro CONST@

    __defined at:__ @macros\/macro_functions.h 5:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST :: Macro.Raw BG.Text
cONST = Macro.functionLike "CONST" ["X", "Y"] ["X"]

{-| __C declaration:__ @macro CONST_3@

    __defined at:__ @macros\/macro_functions.h 6:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST_3 :: Macro.Raw BG.Text
cONST_3 =
  Macro.functionLike "CONST_3" ["X", "Y", "Z"] ["X"]

{-| __C declaration:__ @macro CMP@

    __defined at:__ @macros\/macro_functions.h 8:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cMP :: Macro.Raw BG.Text
cMP =
  Macro.functionLike "CMP" ["X", "Y"] ["X", "<", "Y"]

{-| __C declaration:__ @macro FUN1@

    __defined at:__ @macros\/macro_functions.h 9:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN1 :: Macro.Raw BG.Text
fUN1 =
  Macro.functionLike "FUN1" ["X", "Y"] ["X", "+", "12ull", "*", "Y"]

{-| __C declaration:__ @macro FUN2@

    __defined at:__ @macros\/macro_functions.h 10:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN2 :: Macro.Raw BG.Text
fUN2 =
  Macro.functionLike "FUN2" ["X", "Y"] ["X", "<<", "(", "3ull", "*", "Y", ")"]

{-| __C declaration:__ @macro G@

    __defined at:__ @macros\/macro_functions.h 12:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g :: Macro.Raw BG.Text
g =
  Macro.functionLike "G" ["X", "Y"] ["CONST", "(", "INCR", "(", "Y", ")", ",", "ID", "(", "X", ")", ")"]

{-| __C declaration:__ @macro G_3@

    __defined at:__ @macros\/macro_functions.h 13:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g_3 :: Macro.Raw BG.Text
g_3 =
  Macro.functionLike "G_3" ["X", "Y", "Z"] [ "CONST_3"
                                           , "("
                                           , "INCR"
                                           , "("
                                           , "Y"
                                           , ")"
                                           , ","
                                           , "ID"
                                           , "("
                                           , "X"
                                           , ")"
                                           , ","
                                           , "ID"
                                           , "("
                                           , "Z"
                                           , ")"
                                           , ")"
                                           ]

{-| __C declaration:__ @macro DIV1@

    __defined at:__ @macros\/macro_functions.h 15:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV1 :: Macro.Raw BG.Text
dIV1 =
  Macro.functionLike "DIV1" ["X", "Y"] ["X", "/", "(", "Y", "+", "12u", ")"]

{-| __C declaration:__ @macro DIV2@

    __defined at:__ @macros\/macro_functions.h 16:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV2 :: Macro.Raw BG.Text
dIV2 =
  Macro.functionLike "DIV2" ["X", "Y"] ["10.0f", "*", "X", "/", "Y"]

{-| __C declaration:__ @macro SWAP32@

    __defined at:__ @macros\/macro_functions.h 20:9@

    __exported by:__ @macros\/macro_functions.h@
-}
sWAP32 :: Macro.Raw BG.Text
sWAP32 =
  Macro.functionLike "SWAP32" ["w"] [ "("
                                    , "("
                                    , "("
                                    , "w"
                                    , ")"
                                    , ">>"
                                    , "24"
                                    , ")"
                                    , "&"
                                    , "0xff"
                                    , ")"
                                    , "|"
                                    , "("
                                    , "("
                                    , "("
                                    , "w"
                                    , ")"
                                    , "<<"
                                    , "8"
                                    , ")"
                                    , "&"
                                    , "0xff0000"
                                    , ")"
                                    ]

{-| __C declaration:__ @macro AV_VERSION_INT@

    __defined at:__ @macros\/macro_functions.h 21:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aV_VERSION_INT :: Macro.Raw BG.Text
aV_VERSION_INT =
  Macro.functionLike "AV_VERSION_INT" ["a", "b", "c"] [ "("
                                                      , "("
                                                      , "a"
                                                      , ")"
                                                      , "<<"
                                                      , "16"
                                                      , "|"
                                                      , "("
                                                      , "b"
                                                      , ")"
                                                      , "<<"
                                                      , "8"
                                                      , "|"
                                                      , "("
                                                      , "c"
                                                      , ")"
                                                      , ")"
                                                      ]
