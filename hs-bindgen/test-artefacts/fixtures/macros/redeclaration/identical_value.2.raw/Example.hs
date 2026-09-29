module Example
    ( Example.a
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/redeclaration\/identical_value.h 3:9@

    __exported by:__ @macros\/redeclaration\/identical_value.h@
-}
a :: Macro.Raw String
a = Macro.objectLike "A" ["5"]
