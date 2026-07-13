{-# LANGUAGE ExplicitForAll #-}

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

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro OBJECTLIKE1@

    __defined at:__ @macros\/macros.h 1:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE1 :: BG.CInt
oBJECTLIKE1 = (1 :: BG.CInt)

{-| __C declaration:__ @macro OBJECTLIKE2@

    __defined at:__ @macros\/macros.h 2:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE2 :: BG.CInt
oBJECTLIKE2 = (2 :: BG.CInt)

{-| __C declaration:__ @macro OBJECTLIKE3@

    __defined at:__ @macros\/macros.h 3:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE3 :: BG.CInt
oBJECTLIKE3 =
  (C.Expr.HostPlatform.+) (3 :: BG.CInt) (3 :: BG.CInt)

{-| __C declaration:__ @macro OBJECTLIKE4@

    __defined at:__ @macros\/macros.h 4:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE4 :: BG.CInt
oBJECTLIKE4 =
  (C.Expr.HostPlatform.+) (4 :: BG.CInt) (4 :: BG.CInt)

{-| __C declaration:__ @macro MEANING_OF_LIFE1@

    __defined at:__ @macros\/macros.h 6:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE1 :: BG.CInt
mEANING_OF_LIFE1 = (42 :: BG.CInt)

{-| __C declaration:__ @macro MEANING_OF_LIFE2@

    __defined at:__ @macros\/macros.h 7:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE2 :: BG.CInt
mEANING_OF_LIFE2 = (42 :: BG.CInt)

{-| __C declaration:__ @macro MEANING_OF_LIFE3@

    __defined at:__ @macros\/macros.h 8:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE3 :: BG.CInt
mEANING_OF_LIFE3 = (42 :: BG.CInt)

{-| __C declaration:__ @macro MEANING_OF_LIFE4@

    __defined at:__ @macros\/macros.h 9:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE4 :: BG.CInt
mEANING_OF_LIFE4 = (42 :: BG.CInt)

{-| __C declaration:__ @macro MEANING_OF_LIFE5@

    __defined at:__ @macros\/macros.h 10:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE5 :: BG.CInt
mEANING_OF_LIFE5 = (42 :: BG.CInt)

{-| __C declaration:__ @macro LONG_INT_TOKEN1@

    __defined at:__ @macros\/macros.h 12:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN1 :: BG.CULLong
lONG_INT_TOKEN1 =
  (18446744073709550592 :: BG.CULLong)

{-| __C declaration:__ @macro LONG_INT_TOKEN2@

    __defined at:__ @macros\/macros.h 13:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN2 :: BG.CULLong
lONG_INT_TOKEN2 =
  (18446744073709550592 :: BG.CULLong)

{-| __C declaration:__ @macro LONG_INT_TOKEN3@

    __defined at:__ @macros\/macros.h 14:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN3 :: BG.CULLong
lONG_INT_TOKEN3 =
  (18446744073709550592 :: BG.CULLong)

{-| __C declaration:__ @macro LONG_INT_TOKEN4@

    __defined at:__ @macros\/macros.h 15:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN4 :: BG.CULLong
lONG_INT_TOKEN4 =
  (18446744073709550592 :: BG.CULLong)

{-| __C declaration:__ @macro TUPLE1@

    __defined at:__ @macros\/macros.h 17:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE1 :: (BG.CInt, BG.CInt)
tUPLE1 = ((1 :: BG.CInt), (2 :: BG.CInt))

{-| __C declaration:__ @macro TUPLE2@

    __defined at:__ @macros\/macros.h 18:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE2 :: (BG.CInt, BG.CInt)
tUPLE2 = ((3 :: BG.CInt), (4 :: BG.CInt))

{-| __C declaration:__ @macro TUPLE3@

    __defined at:__ @macros\/macros.h 19:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE3 :: (BG.CInt, BG.CInt)
tUPLE3 = ((5 :: BG.CInt), (6 :: BG.CInt))

{-| __C declaration:__ @macro FLT1_1@

    __defined at:__ @macros\/macros.h 24:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_1 :: BG.CDouble
fLT1_1 = (110000.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT1_2@

    __defined at:__ @macros\/macros.h 25:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_2 :: BG.CDouble
fLT1_2 = (1.2e-2 :: BG.CDouble)

{-| __C declaration:__ @macro FLT1_3@

    __defined at:__ @macros\/macros.h 26:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_3 :: BG.CFloat
fLT1_3 = (1.3e-2 :: BG.CFloat)

{-| __C declaration:__ @macro FLT2_1@

    __defined at:__ @macros\/macros.h 28:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_1 :: BG.CDouble
fLT2_1 = (21.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT2_2@

    __defined at:__ @macros\/macros.h 29:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_2 :: BG.CDouble
fLT2_2 = (2200.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT2_3@

    __defined at:__ @macros\/macros.h 30:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_3 :: BG.CFloat
fLT2_3 = (23.0 :: BG.CFloat)

{-| __C declaration:__ @macro FLT3_1@

    __defined at:__ @macros\/macros.h 32:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_1 :: BG.CDouble
fLT3_1 = (31.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT3_2@

    __defined at:__ @macros\/macros.h 33:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_2 :: BG.CDouble
fLT3_2 = (0.32 :: BG.CDouble)

{-| __C declaration:__ @macro FLT3_3@

    __defined at:__ @macros\/macros.h 34:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_3 :: BG.CDouble
fLT3_3 = (33.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT3_4@

    __defined at:__ @macros\/macros.h 35:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_4 :: BG.CFloat
fLT3_4 = (3.4e-3 :: BG.CFloat)

{-| __C declaration:__ @macro FLT4_1@

    __defined at:__ @macros\/macros.h 37:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_1 :: BG.CDouble
fLT4_1 = (650000.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT4_2@

    __defined at:__ @macros\/macros.h 38:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_2 :: BG.CDouble
fLT4_2 = (6.6e-2 :: BG.CDouble)

{-| __C declaration:__ @macro FLT4_3@

    __defined at:__ @macros\/macros.h 39:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_3 :: BG.CFloat
fLT4_3 = (6.7e-2 :: BG.CFloat)

{-| __C declaration:__ @macro FLT5_1@

    __defined at:__ @macros\/macros.h 41:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_1 :: BG.CDouble
fLT5_1 = (81.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT5_2@

    __defined at:__ @macros\/macros.h 42:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_2 :: BG.CFloat
fLT5_2 = (82.0 :: BG.CFloat)

{-| __C declaration:__ @macro FLT6_1@

    __defined at:__ @macros\/macros.h 44:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_1 :: BG.CDouble
fLT6_1 = (15520.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT6_2@

    __defined at:__ @macros\/macros.h 45:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_2 :: BG.CDouble
fLT6_2 = (98.0 :: BG.CDouble)

{-| __C declaration:__ @macro FLT6_3@

    __defined at:__ @macros\/macros.h 46:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_3 :: BG.CFloat
fLT6_3 = (9.9e-3 :: BG.CFloat)

{-| __C declaration:__ @macro BAD1@

    __defined at:__ @macros\/macros.h 49:9@

    __exported by:__ @macros\/macros.h@
-}
bAD1 :: BG.CDouble
bAD1 =
  (C.Expr.HostPlatform.+) (0.1 :: BG.CDouble) (1 :: BG.CInt)

{-| __C declaration:__ @macro BAD2@

    __defined at:__ @macros\/macros.h 50:9@

    __exported by:__ @macros\/macros.h@
-}
bAD2 :: BG.CULong
bAD2 =
  (C.Expr.HostPlatform.*) (2 :: BG.CLong) (2 :: BG.CULong)
