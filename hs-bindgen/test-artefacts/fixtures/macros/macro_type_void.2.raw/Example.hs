module Example
    ( Example.myVoid
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro MyVoid@

    __defined at:__ @macros\/macro_type_void.h 3:9@

    __exported by:__ @macros\/macro_type_void.h@
-}
myVoid :: Macro.Raw BG.Text
myVoid = Macro.objectLike "MyVoid" ["void"]
