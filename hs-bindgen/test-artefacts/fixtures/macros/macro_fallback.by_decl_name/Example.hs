{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example
    ( Example.hANDLED
    , Example.pARSE_FAILURE
    , Example.tYPE_LIKE
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

{-| __C declaration:__ @macro TYPE_LIKE@

    __defined at:__ @macros\/macro_fallback.h 20:9@

    __exported by:__ @macros\/macro_fallback.h@
-}
tYPE_LIKE :: Macro.Raw String
tYPE_LIKE =
  Macro.objectLike "TYPE_LIKE" ["const", "HANDLED"]
