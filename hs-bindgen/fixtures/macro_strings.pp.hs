{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Char
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import Prelude (Int)

{-| __C declaration:__ @C1@

    __defined at:__ @macro_strings.h:4:9@

    __exported by:__ @macro_strings.h@
-}
c1 :: C.Char.CharValue
c1 = C.Char.charValueFromAddr "a"# 1 (Just 'a')

{-| __C declaration:__ @C2@

    __defined at:__ @macro_strings.h:5:9@

    __exported by:__ @macro_strings.h@
-}
c2 :: C.Char.CharValue
c2 = C.Char.charValueFromAddr "\""# 1 (Just '"')

{-| __C declaration:__ @C3@

    __defined at:__ @macro_strings.h:6:9@

    __exported by:__ @macro_strings.h@
-}
c3 :: C.Char.CharValue
c3 = C.Char.charValueFromAddr "\t"# 1 (Just '\t')

{-| __C declaration:__ @C4@

    __defined at:__ @macro_strings.h:7:9@

    __exported by:__ @macro_strings.h@
-}
c4 :: C.Char.CharValue
c4 = C.Char.charValueFromAddr "\0"# 1 (Just '\NUL')

{-| __C declaration:__ @C5@

    __defined at:__ @macro_strings.h:8:9@

    __exported by:__ @macro_strings.h@
-}
c5 :: C.Char.CharValue
c5 = C.Char.charValueFromAddr "\'"# 1 (Just '\'')

{-| __C declaration:__ @C6@

    __defined at:__ @macro_strings.h:9:9@

    __exported by:__ @macro_strings.h@
-}
c6 :: C.Char.CharValue
c6 = C.Char.charValueFromAddr "?"# 1 (Just '?')

{-| __C declaration:__ @C7@

    __defined at:__ @macro_strings.h:10:9@

    __exported by:__ @macro_strings.h@
-}
c7 :: C.Char.CharValue
c7 = C.Char.charValueFromAddr "S"# 1 Nothing

{-| __C declaration:__ @C8@

    __defined at:__ @macro_strings.h:11:9@

    __exported by:__ @macro_strings.h@
-}
c8 :: C.Char.CharValue
c8 = C.Char.charValueFromAddr "S"# 1 Nothing

{-| __C declaration:__ @D@

    __defined at:__ @macro_strings.h:13:9@

    __exported by:__ @macro_strings.h@
-}
d :: C.Char.CharValue
d = C.Char.charValueFromAddr "\x1\xFF"# 2 Nothing

{-| __C declaration:__ @J1@

    __defined at:__ @macro_strings.h:15:9@

    __exported by:__ @macro_strings.h@
-}
j1 :: C.Char.CharValue
j1 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

{-| __C declaration:__ @J2@

    __defined at:__ @macro_strings.h:16:9@

    __exported by:__ @macro_strings.h@
-}
j2 :: C.Char.CharValue
j2 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

{-| __C declaration:__ @J3@

    __defined at:__ @macro_strings.h:17:9@

    __exported by:__ @macro_strings.h@
-}
j3 :: C.Char.CharValue
j3 =
  C.Char.charValueFromAddr "\xE3\x81\x82"# 3 Nothing

{-| __C declaration:__ @S1@

    __defined at:__ @macro_strings.h:20:9@

    __exported by:__ @macro_strings.h@
-}
s1 :: ((,) (F.Ptr FC.CChar)) Int
s1 = ((F.Ptr "a"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S2@

    __defined at:__ @macro_strings.h:21:9@

    __exported by:__ @macro_strings.h@
-}
s2 :: ((,) (F.Ptr FC.CChar)) Int
s2 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S3@

    __defined at:__ @macro_strings.h:22:9@

    __exported by:__ @macro_strings.h@
-}
s3 :: ((,) (F.Ptr FC.CChar)) Int
s3 = ((F.Ptr "\t"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S4@

    __defined at:__ @macro_strings.h:23:9@

    __exported by:__ @macro_strings.h@
-}
s4 :: ((,) (F.Ptr FC.CChar)) Int
s4 = ((F.Ptr "\0"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S5@

    __defined at:__ @macro_strings.h:24:9@

    __exported by:__ @macro_strings.h@
-}
s5 :: ((,) (F.Ptr FC.CChar)) Int
s5 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S6@

    __defined at:__ @macro_strings.h:25:9@

    __exported by:__ @macro_strings.h@
-}
s6 :: ((,) (F.Ptr FC.CChar)) Int
s6 = ((F.Ptr "?"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S7@

    __defined at:__ @macro_strings.h:26:9@

    __exported by:__ @macro_strings.h@
-}
s7 :: ((,) (F.Ptr FC.CChar)) Int
s7 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @S8@

    __defined at:__ @macro_strings.h:27:9@

    __exported by:__ @macro_strings.h@
-}
s8 :: ((,) (F.Ptr FC.CChar)) Int
s8 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

{-| __C declaration:__ @T1@

    __defined at:__ @macro_strings.h:29:9@

    __exported by:__ @macro_strings.h@
-}
t1 :: ((,) (F.Ptr FC.CChar)) Int
t1 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

{-| __C declaration:__ @T2@

    __defined at:__ @macro_strings.h:30:9@

    __exported by:__ @macro_strings.h@
-}
t2 :: ((,) (F.Ptr FC.CChar)) Int
t2 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

{-| __C declaration:__ @T3@

    __defined at:__ @macro_strings.h:31:9@

    __exported by:__ @macro_strings.h@
-}
t3 :: ((,) (F.Ptr FC.CChar)) Int
t3 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

{-| __C declaration:__ @U@

    __defined at:__ @macro_strings.h:33:9@

    __exported by:__ @macro_strings.h@
-}
u :: ((,) (F.Ptr FC.CChar)) Int
u =
  ((F.Ptr "\x1\xFF\x1\xFF\x1\xFF\x1\xFF"#, 8) :: FC.CStringLen)

{-| __C declaration:__ @V@

    __defined at:__ @macro_strings.h:34:9@

    __exported by:__ @macro_strings.h@
-}
v :: ((,) (F.Ptr FC.CChar)) Int
v =
  ((F.Ptr "\x1\x2\x3\x4\x5\x6"#, 6) :: FC.CStringLen)

{-| __C declaration:__ @W1@

    __defined at:__ @macro_strings.h:36:9@

    __exported by:__ @macro_strings.h@
-}
w1 :: ((,) (F.Ptr FC.CChar)) Int
w1 = ((F.Ptr "hij\0"#, 4) :: FC.CStringLen)

{-| __C declaration:__ @W2@

    __defined at:__ @macro_strings.h:37:9@

    __exported by:__ @macro_strings.h@
-}
w2 :: ((,) (F.Ptr FC.CChar)) Int
w2 = ((F.Ptr "abc\0def\0g"#, 9) :: FC.CStringLen)
