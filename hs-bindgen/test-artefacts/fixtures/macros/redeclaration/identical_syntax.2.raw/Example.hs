module Example
    ( Example.t
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/redeclaration\/identical_syntax.h 5:9@

    __exported by:__ @macros\/redeclaration\/identical_syntax.h@
-}
t :: Macro.Raw String
t = Macro.objectLike "T" ["A"]
