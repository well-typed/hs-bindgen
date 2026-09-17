module Example
    ( Example.myVoid
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro MyVoid@

    __defined at:__ @macros\/macro_type_void.h 3:9@

    __exported by:__ @macros\/macro_type_void.h@
-}
myVoid :: Macro.Raw String
myVoid = Macro.objectLike "MyVoid" ["void"]
