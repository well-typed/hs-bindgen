module Example
    ( Example.b
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/macro_ext_binding_dep.h 6:9@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
b :: Macro.Raw BG.Text
b = Macro.objectLike "B" ["A"]
