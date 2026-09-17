module Example
    ( Example.a
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 16:9@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
a :: Macro.Raw String
a = Macro.objectLike "A" ["int"]
