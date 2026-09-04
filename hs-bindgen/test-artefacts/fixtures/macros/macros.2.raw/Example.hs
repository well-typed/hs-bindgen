module Example
    ( Example.oBJECTLIKE1
    , Example.oBJECTLIKE2
    , Example.oBJECTLIKE3
    , Example.oBJECTLIKE4
    , Example.mEANING_OF_LIFE1
    , Example.mEANING_OF_LIFE2
    , Example.mEANING_OF_LIFE3
    , Example.mEANING_OF_LIFE4
    , Example.mEANING_OF_LIFE5
    , Example.lONG_INT_TOKEN1
    , Example.lONG_INT_TOKEN2
    , Example.lONG_INT_TOKEN3
    , Example.lONG_INT_TOKEN4
    , Example.tUPLE1
    , Example.tUPLE2
    , Example.tUPLE3
    , Example.fLT1_1
    , Example.fLT1_2
    , Example.fLT1_3
    , Example.fLT2_1
    , Example.fLT2_2
    , Example.fLT2_3
    , Example.fLT3_1
    , Example.fLT3_2
    , Example.fLT3_3
    , Example.fLT3_4
    , Example.fLT4_1
    , Example.fLT4_2
    , Example.fLT4_3
    , Example.fLT5_1
    , Example.fLT5_2
    , Example.fLT6_1
    , Example.fLT6_2
    , Example.fLT6_3
    , Example.bAD1
    , Example.bAD2
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro OBJECTLIKE1@

    __defined at:__ @macros\/macros.h 1:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE1 :: Macro.Raw BG.Text
oBJECTLIKE1 = Macro.objectLike "OBJECTLIKE1" ["1"]

{-| __C declaration:__ @macro OBJECTLIKE2@

    __defined at:__ @macros\/macros.h 2:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE2 :: Macro.Raw BG.Text
oBJECTLIKE2 =
  Macro.objectLike "OBJECTLIKE2" ["(", "2", ")"]

{-| __C declaration:__ @macro OBJECTLIKE3@

    __defined at:__ @macros\/macros.h 3:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE3 :: Macro.Raw BG.Text
oBJECTLIKE3 =
  Macro.objectLike "OBJECTLIKE3" ["3", "+", "3"]

{-| __C declaration:__ @macro OBJECTLIKE4@

    __defined at:__ @macros\/macros.h 4:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE4 :: Macro.Raw BG.Text
oBJECTLIKE4 =
  Macro.objectLike "OBJECTLIKE4" ["(", "4", "+", "4", ")"]

{-| __C declaration:__ @macro MEANING_OF_LIFE1@

    __defined at:__ @macros\/macros.h 6:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE1 :: Macro.Raw BG.Text
mEANING_OF_LIFE1 =
  Macro.objectLike "MEANING_OF_LIFE1" ["42"]

{-| __C declaration:__ @macro MEANING_OF_LIFE2@

    __defined at:__ @macros\/macros.h 7:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE2 :: Macro.Raw BG.Text
mEANING_OF_LIFE2 =
  Macro.objectLike "MEANING_OF_LIFE2" ["052"]

{-| __C declaration:__ @macro MEANING_OF_LIFE3@

    __defined at:__ @macros\/macros.h 8:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE3 :: Macro.Raw BG.Text
mEANING_OF_LIFE3 =
  Macro.objectLike "MEANING_OF_LIFE3" ["0x2a"]

{-| __C declaration:__ @macro MEANING_OF_LIFE4@

    __defined at:__ @macros\/macros.h 9:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE4 :: Macro.Raw BG.Text
mEANING_OF_LIFE4 =
  Macro.objectLike "MEANING_OF_LIFE4" ["0X2A"]

{-| __C declaration:__ @macro MEANING_OF_LIFE5@

    __defined at:__ @macros\/macros.h 10:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE5 :: Macro.Raw BG.Text
mEANING_OF_LIFE5 =
  Macro.objectLike "MEANING_OF_LIFE5" ["0b101010"]

{-| __C declaration:__ @macro LONG_INT_TOKEN1@

    __defined at:__ @macros\/macros.h 12:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN1 :: Macro.Raw BG.Text
lONG_INT_TOKEN1 =
  Macro.objectLike "LONG_INT_TOKEN1" ["18446744073709550592ull"]

{-| __C declaration:__ @macro LONG_INT_TOKEN2@

    __defined at:__ @macros\/macros.h 13:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN2 :: Macro.Raw BG.Text
lONG_INT_TOKEN2 =
  Macro.objectLike "LONG_INT_TOKEN2" ["18'446'744'073'709'550'592llu"]

{-| __C declaration:__ @macro LONG_INT_TOKEN3@

    __defined at:__ @macros\/macros.h 14:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN3 :: Macro.Raw BG.Text
lONG_INT_TOKEN3 =
  Macro.objectLike "LONG_INT_TOKEN3" ["1844'6744'0737'0955'0592uLL"]

{-| __C declaration:__ @macro LONG_INT_TOKEN4@

    __defined at:__ @macros\/macros.h 15:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN4 :: Macro.Raw BG.Text
lONG_INT_TOKEN4 =
  Macro.objectLike "LONG_INT_TOKEN4" ["184467'440737'0'95505'92LLU"]

{-| __C declaration:__ @macro TUPLE1@

    __defined at:__ @macros\/macros.h 17:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE1 :: Macro.Raw BG.Text
tUPLE1 =
  Macro.objectLike "TUPLE1" ["(", "1", ",", "2", ")"]

{-| __C declaration:__ @macro TUPLE2@

    __defined at:__ @macros\/macros.h 18:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE2 :: Macro.Raw BG.Text
tUPLE2 = Macro.objectLike "TUPLE2" ["3", ",", "4"]

{-| __C declaration:__ @macro TUPLE3@

    __defined at:__ @macros\/macros.h 19:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE3 :: Macro.Raw BG.Text
tUPLE3 = Macro.objectLike "TUPLE3" ["5", ",", "6"]

{-| __C declaration:__ @macro FLT1_1@

    __defined at:__ @macros\/macros.h 24:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_1 :: Macro.Raw BG.Text
fLT1_1 = Macro.objectLike "FLT1_1" ["11e4"]

{-| __C declaration:__ @macro FLT1_2@

    __defined at:__ @macros\/macros.h 25:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_2 :: Macro.Raw BG.Text
fLT1_2 = Macro.objectLike "FLT1_2" ["12E-3"]

{-| __C declaration:__ @macro FLT1_3@

    __defined at:__ @macros\/macros.h 26:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_3 :: Macro.Raw BG.Text
fLT1_3 = Macro.objectLike "FLT1_3" ["13e-03f"]

{-| __C declaration:__ @macro FLT2_1@

    __defined at:__ @macros\/macros.h 28:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_1 :: Macro.Raw BG.Text
fLT2_1 = Macro.objectLike "FLT2_1" ["21."]

{-| __C declaration:__ @macro FLT2_2@

    __defined at:__ @macros\/macros.h 29:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_2 :: Macro.Raw BG.Text
fLT2_2 = Macro.objectLike "FLT2_2" ["22.e2"]

{-| __C declaration:__ @macro FLT2_3@

    __defined at:__ @macros\/macros.h 30:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_3 :: Macro.Raw BG.Text
fLT2_3 = Macro.objectLike "FLT2_3" ["23.f"]

{-| __C declaration:__ @macro FLT3_1@

    __defined at:__ @macros\/macros.h 32:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_1 :: Macro.Raw BG.Text
fLT3_1 = Macro.objectLike "FLT3_1" ["31.0"]

{-| __C declaration:__ @macro FLT3_2@

    __defined at:__ @macros\/macros.h 33:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_2 :: Macro.Raw BG.Text
fLT3_2 = Macro.objectLike "FLT3_2" [".32"]

{-| __C declaration:__ @macro FLT3_3@

    __defined at:__ @macros\/macros.h 34:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_3 :: Macro.Raw BG.Text
fLT3_3 = Macro.objectLike "FLT3_3" [".33e2"]

{-| __C declaration:__ @macro FLT3_4@

    __defined at:__ @macros\/macros.h 35:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_4 :: Macro.Raw BG.Text
fLT3_4 = Macro.objectLike "FLT3_4" [".34e-2f"]

{-| __C declaration:__ @macro FLT4_1@

    __defined at:__ @macros\/macros.h 37:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_1 :: Macro.Raw BG.Text
fLT4_1 = Macro.objectLike "FLT4_1" ["0x41p4"]

{-| __C declaration:__ @macro FLT4_2@

    __defined at:__ @macros\/macros.h 38:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_2 :: Macro.Raw BG.Text
fLT4_2 = Macro.objectLike "FLT4_2" ["0x42P-3"]

{-| __C declaration:__ @macro FLT4_3@

    __defined at:__ @macros\/macros.h 39:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_3 :: Macro.Raw BG.Text
fLT4_3 = Macro.objectLike "FLT4_3" ["0x43p-03f"]

{-| __C declaration:__ @macro FLT5_1@

    __defined at:__ @macros\/macros.h 41:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_1 :: Macro.Raw BG.Text
fLT5_1 = Macro.objectLike "FLT5_1" ["0x51.p0"]

{-| __C declaration:__ @macro FLT5_2@

    __defined at:__ @macros\/macros.h 42:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_2 :: Macro.Raw BG.Text
fLT5_2 = Macro.objectLike "FLT5_2" ["0x52.P0f"]

{-| __C declaration:__ @macro FLT6_1@

    __defined at:__ @macros\/macros.h 44:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_1 :: Macro.Raw BG.Text
fLT6_1 = Macro.objectLike "FLT6_1" ["0x61.0P2"]

{-| __C declaration:__ @macro FLT6_2@

    __defined at:__ @macros\/macros.h 45:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_2 :: Macro.Raw BG.Text
fLT6_2 = Macro.objectLike "FLT6_2" ["0x.62p2"]

{-| __C declaration:__ @macro FLT6_3@

    __defined at:__ @macros\/macros.h 46:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_3 :: Macro.Raw BG.Text
fLT6_3 = Macro.objectLike "FLT6_3" ["0x.63p-2f"]

{-| __C declaration:__ @macro BAD1@

    __defined at:__ @macros\/macros.h 49:9@

    __exported by:__ @macros\/macros.h@
-}
bAD1 :: Macro.Raw BG.Text
bAD1 = Macro.objectLike "BAD1" ["0.1", "+", "1"]

{-| __C declaration:__ @macro BAD2@

    __defined at:__ @macros\/macros.h 50:9@

    __exported by:__ @macros\/macros.h@
-}
bAD2 :: Macro.Raw BG.Text
bAD2 = Macro.objectLike "BAD2" ["2l", "*", "2ul"]
