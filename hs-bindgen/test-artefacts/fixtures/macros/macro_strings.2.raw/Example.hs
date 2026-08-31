module Example
    ( Example.c1
    , Example.c2
    , Example.c3
    , Example.c4
    , Example.c5
    , Example.c6
    , Example.c7
    , Example.c8
    , Example.d
    , Example.j1
    , Example.j2
    , Example.j3
    , Example.s1
    , Example.s2
    , Example.s3
    , Example.s4
    , Example.s5
    , Example.s6
    , Example.s7
    , Example.s8
    , Example.t1
    , Example.t2
    , Example.t3
    , Example.u
    , Example.v
    , Example.w1
    , Example.w2
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro C1@

    __defined at:__ @macros\/macro_strings.h 4:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c1 :: Macro.Raw BG.Text
c1 = Macro.objectLike "C1" ["'a'"]

{-| __C declaration:__ @macro C2@

    __defined at:__ @macros\/macro_strings.h 5:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c2 :: Macro.Raw BG.Text
c2 = Macro.objectLike "C2" ["'\"'"]

{-| __C declaration:__ @macro C3@

    __defined at:__ @macros\/macro_strings.h 6:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c3 :: Macro.Raw BG.Text
c3 = Macro.objectLike "C3" ["'\\t'"]

{-| __C declaration:__ @macro C4@

    __defined at:__ @macros\/macro_strings.h 7:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c4 :: Macro.Raw BG.Text
c4 = Macro.objectLike "C4" ["'\\0'"]

{-| __C declaration:__ @macro C5@

    __defined at:__ @macros\/macro_strings.h 8:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c5 :: Macro.Raw BG.Text
c5 = Macro.objectLike "C5" ["'\\''"]

{-| __C declaration:__ @macro C6@

    __defined at:__ @macros\/macro_strings.h 9:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c6 :: Macro.Raw BG.Text
c6 = Macro.objectLike "C6" ["'\\?'"]

{-| __C declaration:__ @macro C7@

    __defined at:__ @macros\/macro_strings.h 10:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c7 :: Macro.Raw BG.Text
c7 = Macro.objectLike "C7" ["'\\123'"]

{-| __C declaration:__ @macro C8@

    __defined at:__ @macros\/macro_strings.h 11:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c8 :: Macro.Raw BG.Text
c8 = Macro.objectLike "C8" ["'\\x53'"]

{-| __C declaration:__ @macro D@

    __defined at:__ @macros\/macro_strings.h 13:9@

    __exported by:__ @macros\/macro_strings.h@
-}
d :: Macro.Raw BG.Text
d = Macro.objectLike "D" ["'\\777'"]

{-| __C declaration:__ @macro J1@

    __defined at:__ @macros\/macro_strings.h 15:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j1 :: Macro.Raw BG.Text
j1 = Macro.objectLike "J1" ["'\12354'"]

{-| __C declaration:__ @macro J2@

    __defined at:__ @macros\/macro_strings.h 16:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j2 :: Macro.Raw BG.Text
j2 = Macro.objectLike "J2" ["'\\u3042'"]

{-| __C declaration:__ @macro J3@

    __defined at:__ @macros\/macro_strings.h 17:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j3 :: Macro.Raw BG.Text
j3 = Macro.objectLike "J3" ["'\\xE3\\x81\\x82'"]

{-| __C declaration:__ @macro S1@

    __defined at:__ @macros\/macro_strings.h 20:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s1 :: Macro.Raw BG.Text
s1 = Macro.objectLike "S1" ["\"a\""]

{-| __C declaration:__ @macro S2@

    __defined at:__ @macros\/macro_strings.h 21:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s2 :: Macro.Raw BG.Text
s2 = Macro.objectLike "S2" ["\"'\""]

{-| __C declaration:__ @macro S3@

    __defined at:__ @macros\/macro_strings.h 22:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s3 :: Macro.Raw BG.Text
s3 = Macro.objectLike "S3" ["\"\\t\""]

{-| __C declaration:__ @macro S4@

    __defined at:__ @macros\/macro_strings.h 23:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s4 :: Macro.Raw BG.Text
s4 = Macro.objectLike "S4" ["\"\\0\""]

{-| __C declaration:__ @macro S5@

    __defined at:__ @macros\/macro_strings.h 24:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s5 :: Macro.Raw BG.Text
s5 = Macro.objectLike "S5" ["\"\\'\""]

{-| __C declaration:__ @macro S6@

    __defined at:__ @macros\/macro_strings.h 25:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s6 :: Macro.Raw BG.Text
s6 = Macro.objectLike "S6" ["\"\\?\""]

{-| __C declaration:__ @macro S7@

    __defined at:__ @macros\/macro_strings.h 26:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s7 :: Macro.Raw BG.Text
s7 = Macro.objectLike "S7" ["\"\\123\""]

{-| __C declaration:__ @macro S8@

    __defined at:__ @macros\/macro_strings.h 27:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s8 :: Macro.Raw BG.Text
s8 = Macro.objectLike "S8" ["\"\\x53\""]

{-| __C declaration:__ @macro T1@

    __defined at:__ @macros\/macro_strings.h 29:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t1 :: Macro.Raw BG.Text
t1 = Macro.objectLike "T1" ["\"\12354\""]

{-| __C declaration:__ @macro T2@

    __defined at:__ @macros\/macro_strings.h 30:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t2 :: Macro.Raw BG.Text
t2 = Macro.objectLike "T2" ["\"\\u3042\""]

{-| __C declaration:__ @macro T3@

    __defined at:__ @macros\/macro_strings.h 31:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t3 :: Macro.Raw BG.Text
t3 = Macro.objectLike "T3" ["\"\\xE3\\x81\\x82\""]

{-| __C declaration:__ @macro U@

    __defined at:__ @macros\/macro_strings.h 33:9@

    __exported by:__ @macros\/macro_strings.h@
-}
u :: Macro.Raw BG.Text
u = Macro.objectLike "U" ["\"\\777\\777\\777\\777\""]

{-| __C declaration:__ @macro V@

    __defined at:__ @macros\/macro_strings.h 34:9@

    __exported by:__ @macros\/macro_strings.h@
-}
v :: Macro.Raw BG.Text
v = Macro.objectLike "V" ["\"\\1\\2\\3\\4\\5\\6\""]

{-| __C declaration:__ @macro W1@

    __defined at:__ @macros\/macro_strings.h 36:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w1 :: Macro.Raw BG.Text
w1 = Macro.objectLike "W1" ["\"hij\\0\""]

{-| __C declaration:__ @macro W2@

    __defined at:__ @macros\/macro_strings.h 37:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w2 :: Macro.Raw BG.Text
w2 = Macro.objectLike "W2" ["\"abc\\0def\\0g\""]
