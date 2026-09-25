module Example
    ( Example.v
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro V@

    __defined at:__ @macros\/redeclaration\/main_header.h 5:9@

    __exported by:__ @macros\/redeclaration\/main_header.h@
-}
v :: Macro.Raw String
v = Macro.objectLike "V" ["3"]
