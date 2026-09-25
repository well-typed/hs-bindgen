module Example
    ( Example.g
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro G@

    __defined at:__ @noguard_inner.h 2:9@

    __exported by:__ @macros\/redeclaration\/noguard.h@
-}
g :: Macro.Raw String
g = Macro.objectLike "G" ["9"]
