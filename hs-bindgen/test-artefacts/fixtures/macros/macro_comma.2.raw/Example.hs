module Example
    ( Example.oBJ
    , Example.oBJ_NO_PARENS
    , Example.fUN
    , Example.fUN_THREE
    , Example.aRITH
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro OBJ@

    __defined at:__ @macros\/macro_comma.h 8:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ :: Macro.Raw BG.Text
oBJ =
  Macro.objectLike "OBJ" ["(", "1", ",", "2", ")"]

{-| __C declaration:__ @macro OBJ_NO_PARENS@

    __defined at:__ @macros\/macro_comma.h 9:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ_NO_PARENS :: Macro.Raw BG.Text
oBJ_NO_PARENS =
  Macro.objectLike "OBJ_NO_PARENS" ["1", ",", "2"]

{-| __C declaration:__ @macro FUN@

    __defined at:__ @macros\/macro_comma.h 10:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN :: Macro.Raw BG.Text
fUN =
  Macro.functionLike "FUN" ["x", "y"] ["(", "x", ",", "y", ")"]

{-| __C declaration:__ @macro FUN_THREE@

    __defined at:__ @macros\/macro_comma.h 11:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN_THREE :: Macro.Raw BG.Text
fUN_THREE =
  Macro.functionLike "FUN_THREE" ["x", "y", "z"] ["(", "(", "x", ")", ",", "(", "y", ")", ",", "(", "z", ")", ")"]

{-| __C declaration:__ @macro ARITH@

    __defined at:__ @macros\/macro_comma.h 15:9@

    __exported by:__ @macros\/macro_comma.h@
-}
aRITH :: Macro.Raw BG.Text
aRITH =
  Macro.objectLike "ARITH" ["(", "(", "1", ",", "2", ")", "+", "3", ")"]
