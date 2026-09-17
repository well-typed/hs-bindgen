module Example
    ( Example.i
    , Example.c
    , Example.f
    , Example.l
    , Example.s
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
i :: Macro.Raw String
i = Macro.objectLike "I" ["int"]

{-| __C declaration:__ @macro C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
c :: Macro.Raw String
c = Macro.objectLike "C" ["char"]

{-| __C declaration:__ @macro F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
f :: Macro.Raw String
f = Macro.objectLike "F" ["float"]

{-| __C declaration:__ @macro L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
l :: Macro.Raw String
l = Macro.objectLike "L" ["long"]

{-| __C declaration:__ @macro S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
s :: Macro.Raw String
s = Macro.objectLike "S" ["short"]
