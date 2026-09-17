module Example
    ( Example.b
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/macro_ext_binding_dep.h 6:9@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
b :: Macro.Raw String
b = Macro.objectLike "B" ["A"]
