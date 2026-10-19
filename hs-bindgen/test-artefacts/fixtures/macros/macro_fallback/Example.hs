{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example
    ( Example.hANDLED
    , Example.pARSE_FAILURE
    , Example.rESOLVE_FAILURE
    , Example.tYPECHECK_FAILURE
    , Example.tYPE_LIKE
    , Example.cASCADE
    , Example.oUTSIDE_DEP
    , Example.cONCAT
    , Example.lOG
    , Example.lOG_NAMED
    , Example.eMPTY
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG
import Prelude (String)

{-| __C declaration:__ @macro HANDLED@

    __defined at:__ @macros\/macro_fallback.h 4:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
hANDLED :: BG.CInt
hANDLED = (42 :: BG.CInt)

{-| Fails to parse.

    A fallback binding keeps its Doxygen comment.

    __C declaration:__ @macro PARSE_FAILURE@

    __defined at:__ @macros\/macro_fallback.h 11:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
pARSE_FAILURE :: Macro.Raw String
pARSE_FAILURE =
  Macro.objectLike "PARSE_FAILURE" ["__attribute__", "(", "(", "unused", ")", ")"]

{-| __C declaration:__ @macro RESOLVE_FAILURE@

    __defined at:__ @macros\/macro_fallback.h 14:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
rESOLVE_FAILURE :: Macro.Raw String
rESOLVE_FAILURE =
  Macro.objectLike "RESOLVE_FAILURE" ["(", "undeclared", "+", "1", ")"]

{-| __C declaration:__ @macro TYPECHECK_FAILURE@

    __defined at:__ @macros\/macro_fallback.h 17:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
tYPECHECK_FAILURE :: Macro.Raw String
tYPECHECK_FAILURE =
  Macro.objectLike "TYPECHECK_FAILURE" ["(", "HANDLED", "+", "\"x\"", ")"]

{-| __C declaration:__ @macro TYPE_LIKE@

    __defined at:__ @macros\/macro_fallback.h 20:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
tYPE_LIKE :: Macro.Raw String
tYPE_LIKE =
  Macro.objectLike "TYPE_LIKE" ["const", "HANDLED"]

{-| __C declaration:__ @macro CASCADE@

    __defined at:__ @macros\/macro_fallback.h 23:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
cASCADE :: Macro.Raw String
cASCADE =
  Macro.objectLike "CASCADE" ["(", "PARSE_FAILURE", "+", "1", ")"]

{-| __C declaration:__ @macro OUTSIDE_DEP@

    __defined at:__ @macros\/macro_fallback.h 28:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
oUTSIDE_DEP :: Macro.Raw String
oUTSIDE_DEP =
  Macro.objectLike "OUTSIDE_DEP" ["(", "outside_t", "+", "1", ")"]

{-| __C declaration:__ @macro CONCAT@

    __defined at:__ @macros\/macro_fallback.h 31:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
cONCAT :: Macro.Raw String
cONCAT =
  Macro.functionLike "CONCAT" ["x", "y"] ["x", "##", "y"]

{-| __C declaration:__ @macro LOG@

    __defined at:__ @macros\/macro_fallback.h 32:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
lOG :: Macro.Raw String
lOG =
  Macro.variadic "LOG" ["fmt"] ["printf", "(", "fmt", ",", "__VA_ARGS__", ")"]

{-| __C declaration:__ @macro LOG_NAMED@

    __defined at:__ @macros\/macro_fallback.h 33:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
lOG_NAMED :: Macro.Raw String
lOG_NAMED =
  Macro.variadicNamed "LOG_NAMED" ["fmt"] "args" ["printf", "(", "fmt", ",", "args", ")"]

{-| __C declaration:__ @macro EMPTY@

    __defined at:__ @macros\/macro_fallback.h 37:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
eMPTY :: Macro.Raw String
eMPTY = Macro.objectLike "EMPTY" []
