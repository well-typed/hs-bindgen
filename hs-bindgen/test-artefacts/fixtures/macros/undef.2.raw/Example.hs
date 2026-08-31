module Example
    ( Example.t
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/undef.h 3:9@

    __exported by:__ @macros\/undef.h@
-}
t :: Macro.Raw BG.Text
t = Macro.objectLike "T" ["int"]
