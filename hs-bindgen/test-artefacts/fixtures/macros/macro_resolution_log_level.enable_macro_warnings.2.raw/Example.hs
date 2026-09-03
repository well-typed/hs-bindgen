module Example
    ( Example.uNRESOLVED_MACRO
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro UNRESOLVED_MACRO@

    __defined at:__ @macros\/macro_resolution_log_level.h 7:9@

    __exported by:__ @macros\/macro_resolution_log_level.h@
-}
uNRESOLVED_MACRO :: Macro.Raw BG.Text
uNRESOLVED_MACRO =
  Macro.objectLike "UNRESOLVED_MACRO" ["struct", "DoesNotExist"]
