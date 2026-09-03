module Example
    ( Example.a
    , Example.b
    , Example.c
    , Example.d
    , Example.e
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/issue_890.h 1:9@

    __exported by:__ @macros\/issue_890.h@
-}
a :: Macro.Raw BG.Text
a = Macro.objectLike "A" ["0"]

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/issue_890.h 2:9@

    __exported by:__ @macros\/issue_890.h@
-}
b :: Macro.Raw BG.Text
b =
  Macro.functionLike "B" ["x"] ["(", "x", "+", "1", ")"]

{-| __C declaration:__ @macro C@

    __defined at:__ @macros\/issue_890.h 3:9@

    __exported by:__ @macros\/issue_890.h@
-}
c :: Macro.Raw BG.Text
c = Macro.objectLike "C" ["B", "(", "0", ")"]

{-| __C declaration:__ @macro D@

    __defined at:__ @macros\/issue_890.h 4:9@

    __exported by:__ @macros\/issue_890.h@
-}
d :: Macro.Raw BG.Text
d = Macro.objectLike "D" ["B", "(", "A", ")"]

{-| __C declaration:__ @macro E@

    __defined at:__ @macros\/issue_890.h 5:9@

    __exported by:__ @macros\/issue_890.h@
-}
e :: Macro.Raw BG.Text
e = Macro.objectLike "E" ["B", "(", "1", ")"]
