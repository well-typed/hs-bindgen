module Example
    ( Example.iD_INT
    , Example.aDD_SIZEOF
    , Example.fST_CONST
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro ID_INT@

    __defined at:__ @macros\/keyword_params.h 9:9@

    __exported by:__ @macros\/keyword_params.h@
-}
iD_INT :: Macro.Raw String
iD_INT = Macro.functionLike "ID_INT" ["int"] ["int"]

{-| __C declaration:__ @macro ADD_SIZEOF@

    __defined at:__ @macros\/keyword_params.h 10:9@

    __exported by:__ @macros\/keyword_params.h@
-}
aDD_SIZEOF :: Macro.Raw String
aDD_SIZEOF =
  Macro.functionLike "ADD_SIZEOF" ["sizeof", "x"] ["sizeof", "+", "x"]

{-| __C declaration:__ @macro FST_CONST@

    __defined at:__ @macros\/keyword_params.h 11:9@

    __exported by:__ @macros\/keyword_params.h@
-}
fST_CONST :: Macro.Raw String
fST_CONST =
  Macro.functionLike "FST_CONST" ["const", "volatile"] ["const"]
