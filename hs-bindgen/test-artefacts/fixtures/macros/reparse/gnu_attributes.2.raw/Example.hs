module Example
    ( Example.bOOL
    , Example.eXPORT
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro BOOL@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 2:9@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bOOL :: Macro.Raw BG.Text
bOOL = Macro.objectLike "BOOL" ["int"]

{-| __C declaration:__ @macro EXPORT@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 10:9@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
eXPORT :: Macro.Raw BG.Text
eXPORT =
  Macro.objectLike "EXPORT" ["__attribute__", "(", "(", "visibility", "(", "\"default\"", ")", ")", ")"]
