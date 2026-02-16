{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}

module Example where

import qualified C.Char
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @C1@

    __defined at:__ @macros\/macro_strings.h 4:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c1 :: C.Char.CharValue
c1 = C.Char.charValueFromAddr "a"# 1 (Just 'a')

{-| __C declaration:__ @C2@

    __defined at:__ @macros\/macro_strings.h 5:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c2 :: C.Char.CharValue
c2 = C.Char.charValueFromAddr "\""# 1 (Just '"')

{-| __C declaration:__ @C3@

    __defined at:__ @macros\/macro_strings.h 6:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c3 :: C.Char.CharValue
c3 = C.Char.charValueFromAddr "\t"# 1 (Just '\t')

{-| __C declaration:__ @C4@

    __defined at:__ @macros\/macro_strings.h 7:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c4 :: C.Char.CharValue
c4 = C.Char.charValueFromAddr "\0"# 1 (Just '\NUL')

{-| __C declaration:__ @C5@

    __defined at:__ @macros\/macro_strings.h 8:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c5 :: C.Char.CharValue
c5 = C.Char.charValueFromAddr "\'"# 1 (Just '\'')

{-| __C declaration:__ @C6@

    __defined at:__ @macros\/macro_strings.h 9:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c6 :: C.Char.CharValue
c6 = C.Char.charValueFromAddr "?"# 1 (Just '?')

{-| __C declaration:__ @C7@

    __defined at:__ @macros\/macro_strings.h 10:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c7 :: C.Char.CharValue
c7 = C.Char.charValueFromAddr "S"# 1 Nothing

{-| __C declaration:__ @C8@

    __defined at:__ @macros\/macro_strings.h 11:9@

    __exported by:__ @macros\/macro_strings.h@
-}
c8 :: C.Char.CharValue
c8 = C.Char.charValueFromAddr "S"# 1 Nothing

{-| __C declaration:__ @D@

    __defined at:__ @macros\/macro_strings.h 13:9@

    __exported by:__ @macros\/macro_strings.h@
-}
d :: C.Char.CharValue
d = C.Char.charValueFromAddr "\x1\xFF"# 2 Nothing

{-| __C declaration:__ @J1@

    __defined at:__ @macros\/macro_strings.h 15:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j1 :: C.Char.CharValue
j1 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

{-| __C declaration:__ @J2@

    __defined at:__ @macros\/macro_strings.h 16:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j2 :: C.Char.CharValue
j2 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

{-| __C declaration:__ @J3@

    __defined at:__ @macros\/macro_strings.h 17:9@

    __exported by:__ @macros\/macro_strings.h@
-}
j3 :: C.Char.CharValue
j3 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 Nothing

{-| __C declaration:__ @S1@

    __defined at:__ @macros\/macro_strings.h 20:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s1 :: ((,) (RIP.Ptr RIP.CChar)) Int
s1 = ((RIP.Ptr "a"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S2@

    __defined at:__ @macros\/macro_strings.h 21:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s2 :: ((,) (RIP.Ptr RIP.CChar)) Int
s2 = ((RIP.Ptr "\'"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S3@

    __defined at:__ @macros\/macro_strings.h 22:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s3 :: ((,) (RIP.Ptr RIP.CChar)) Int
s3 = ((RIP.Ptr "\t"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S4@

    __defined at:__ @macros\/macro_strings.h 23:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s4 :: ((,) (RIP.Ptr RIP.CChar)) Int
s4 = ((RIP.Ptr "\0"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S5@

    __defined at:__ @macros\/macro_strings.h 24:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s5 :: ((,) (RIP.Ptr RIP.CChar)) Int
s5 = ((RIP.Ptr "\'"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S6@

    __defined at:__ @macros\/macro_strings.h 25:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s6 :: ((,) (RIP.Ptr RIP.CChar)) Int
s6 = ((RIP.Ptr "?"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S7@

    __defined at:__ @macros\/macro_strings.h 26:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s7 :: ((,) (RIP.Ptr RIP.CChar)) Int
s7 = ((RIP.Ptr "S"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @S8@

    __defined at:__ @macros\/macro_strings.h 27:9@

    __exported by:__ @macros\/macro_strings.h@
-}
s8 :: ((,) (RIP.Ptr RIP.CChar)) Int
s8 = ((RIP.Ptr "S"#, 1) :: RIP.CStringLen)

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/macro_strings.h 29:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t1 :: ((,) (RIP.Ptr RIP.CChar)) Int
t1 = ((RIP.Ptr "\xE3\x81\x82"#, 3) :: RIP.CStringLen)

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/macro_strings.h 30:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t2 :: ((,) (RIP.Ptr RIP.CChar)) Int
t2 = ((RIP.Ptr "\xE3\x81\x82"#, 3) :: RIP.CStringLen)

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/macro_strings.h 31:9@

    __exported by:__ @macros\/macro_strings.h@
-}
t3 :: ((,) (RIP.Ptr RIP.CChar)) Int
t3 = ((RIP.Ptr "\xE3\x81\x82"#, 3) :: RIP.CStringLen)

{-| __C declaration:__ @U@

    __defined at:__ @macros\/macro_strings.h 33:9@

    __exported by:__ @macros\/macro_strings.h@
-}
u :: ((,) (RIP.Ptr RIP.CChar)) Int
u =
  ((RIP.Ptr "\x1\xFF\x1\xFF\x1\xFF\x1\xFF"#, 8) :: RIP.CStringLen)

{-| __C declaration:__ @V@

    __defined at:__ @macros\/macro_strings.h 34:9@

    __exported by:__ @macros\/macro_strings.h@
-}
v :: ((,) (RIP.Ptr RIP.CChar)) Int
v =
  ((RIP.Ptr "\x1\x2\x3\x4\x5\x6"#, 6) :: RIP.CStringLen)

{-| __C declaration:__ @W1@

    __defined at:__ @macros\/macro_strings.h 36:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w1 :: ((,) (RIP.Ptr RIP.CChar)) Int
w1 = ((RIP.Ptr "hij\0"#, 4) :: RIP.CStringLen)

{-| __C declaration:__ @W2@

    __defined at:__ @macros\/macro_strings.h 37:9@

    __exported by:__ @macros\/macro_strings.h@
-}
w2 :: ((,) (RIP.Ptr RIP.CChar)) Int
w2 = ((RIP.Ptr "abc\0def\0g"#, 9) :: RIP.CStringLen)
