module Example
    ( Example.a
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 16:9@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
a :: Macro.Raw BG.Text
a = Macro.objectLike "A" ["int"]
