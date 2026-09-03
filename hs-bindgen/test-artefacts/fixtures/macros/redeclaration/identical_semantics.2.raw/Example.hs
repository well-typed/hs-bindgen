module Example
    ( Example.t
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 2:9@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
t :: Macro.Raw BG.Text
t = Macro.objectLike "T" ["int"]
