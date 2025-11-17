{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Expr.HostPlatform as C
import qualified Foreign.C as FC

{-| __C declaration:__ @OBJECTLIKE1@

    __defined at:__ @macros\/macros.h:1:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE1 :: FC.CInt
oBJECTLIKE1 = (1 :: FC.CInt)

{-| __C declaration:__ @OBJECTLIKE2@

    __defined at:__ @macros\/macros.h:2:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE2 :: FC.CInt
oBJECTLIKE2 = (2 :: FC.CInt)

{-| __C declaration:__ @OBJECTLIKE3@

    __defined at:__ @macros\/macros.h:3:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE3 :: FC.CInt
oBJECTLIKE3 = (C.+) (3 :: FC.CInt) (3 :: FC.CInt)

{-| __C declaration:__ @OBJECTLIKE4@

    __defined at:__ @macros\/macros.h:4:9@

    __exported by:__ @macros\/macros.h@
-}
oBJECTLIKE4 :: FC.CInt
oBJECTLIKE4 = (C.+) (4 :: FC.CInt) (4 :: FC.CInt)

{-| __C declaration:__ @MEANING_OF_LIFE1@

    __defined at:__ @macros\/macros.h:6:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE1 :: FC.CInt
mEANING_OF_LIFE1 = (42 :: FC.CInt)

{-| __C declaration:__ @MEANING_OF_LIFE2@

    __defined at:__ @macros\/macros.h:7:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE2 :: FC.CInt
mEANING_OF_LIFE2 = (42 :: FC.CInt)

{-| __C declaration:__ @MEANING_OF_LIFE3@

    __defined at:__ @macros\/macros.h:8:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE3 :: FC.CInt
mEANING_OF_LIFE3 = (42 :: FC.CInt)

{-| __C declaration:__ @MEANING_OF_LIFE4@

    __defined at:__ @macros\/macros.h:9:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE4 :: FC.CInt
mEANING_OF_LIFE4 = (42 :: FC.CInt)

{-| __C declaration:__ @MEANING_OF_LIFE5@

    __defined at:__ @macros\/macros.h:10:9@

    __exported by:__ @macros\/macros.h@
-}
mEANING_OF_LIFE5 :: FC.CInt
mEANING_OF_LIFE5 = (42 :: FC.CInt)

{-| __C declaration:__ @LONG_INT_TOKEN1@

    __defined at:__ @macros\/macros.h:12:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN1 :: FC.CULLong
lONG_INT_TOKEN1 =
  (18446744073709550592 :: FC.CULLong)

{-| __C declaration:__ @LONG_INT_TOKEN2@

    __defined at:__ @macros\/macros.h:13:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN2 :: FC.CULLong
lONG_INT_TOKEN2 =
  (18446744073709550592 :: FC.CULLong)

{-| __C declaration:__ @LONG_INT_TOKEN3@

    __defined at:__ @macros\/macros.h:14:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN3 :: FC.CULLong
lONG_INT_TOKEN3 =
  (18446744073709550592 :: FC.CULLong)

{-| __C declaration:__ @LONG_INT_TOKEN4@

    __defined at:__ @macros\/macros.h:15:9@

    __exported by:__ @macros\/macros.h@
-}
lONG_INT_TOKEN4 :: FC.CULLong
lONG_INT_TOKEN4 =
  (18446744073709550592 :: FC.CULLong)

{-| __C declaration:__ @TUPLE1@

    __defined at:__ @macros\/macros.h:17:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE1 :: ((,) FC.CInt) FC.CInt
tUPLE1 = (,) (1 :: FC.CInt) (2 :: FC.CInt)

{-| __C declaration:__ @TUPLE2@

    __defined at:__ @macros\/macros.h:18:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE2 :: ((,) FC.CInt) FC.CInt
tUPLE2 = (,) (3 :: FC.CInt) (4 :: FC.CInt)

{-| __C declaration:__ @TUPLE3@

    __defined at:__ @macros\/macros.h:19:9@

    __exported by:__ @macros\/macros.h@
-}
tUPLE3 :: ((,) FC.CInt) FC.CInt
tUPLE3 = (,) (5 :: FC.CInt) (6 :: FC.CInt)

{-| __C declaration:__ @FLT1_1@

    __defined at:__ @macros\/macros.h:24:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_1 :: FC.CDouble
fLT1_1 = (110000.0 :: FC.CDouble)

{-| __C declaration:__ @FLT1_2@

    __defined at:__ @macros\/macros.h:25:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_2 :: FC.CDouble
fLT1_2 = (1.2e-2 :: FC.CDouble)

{-| __C declaration:__ @FLT1_3@

    __defined at:__ @macros\/macros.h:26:9@

    __exported by:__ @macros\/macros.h@
-}
fLT1_3 :: FC.CFloat
fLT1_3 = (1.3e-2 :: FC.CFloat)

{-| __C declaration:__ @FLT2_1@

    __defined at:__ @macros\/macros.h:28:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_1 :: FC.CDouble
fLT2_1 = (21.0 :: FC.CDouble)

{-| __C declaration:__ @FLT2_2@

    __defined at:__ @macros\/macros.h:29:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_2 :: FC.CDouble
fLT2_2 = (2200.0 :: FC.CDouble)

{-| __C declaration:__ @FLT2_3@

    __defined at:__ @macros\/macros.h:30:9@

    __exported by:__ @macros\/macros.h@
-}
fLT2_3 :: FC.CFloat
fLT2_3 = (23.0 :: FC.CFloat)

{-| __C declaration:__ @FLT3_1@

    __defined at:__ @macros\/macros.h:32:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_1 :: FC.CDouble
fLT3_1 = (31.0 :: FC.CDouble)

{-| __C declaration:__ @FLT3_2@

    __defined at:__ @macros\/macros.h:33:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_2 :: FC.CDouble
fLT3_2 = (0.32 :: FC.CDouble)

{-| __C declaration:__ @FLT3_3@

    __defined at:__ @macros\/macros.h:34:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_3 :: FC.CDouble
fLT3_3 = (33.0 :: FC.CDouble)

{-| __C declaration:__ @FLT3_4@

    __defined at:__ @macros\/macros.h:35:9@

    __exported by:__ @macros\/macros.h@
-}
fLT3_4 :: FC.CFloat
fLT3_4 = (3.4e-3 :: FC.CFloat)

{-| __C declaration:__ @FLT4_1@

    __defined at:__ @macros\/macros.h:37:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_1 :: FC.CDouble
fLT4_1 = (650000.0 :: FC.CDouble)

{-| __C declaration:__ @FLT4_2@

    __defined at:__ @macros\/macros.h:38:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_2 :: FC.CDouble
fLT4_2 = (6.6e-2 :: FC.CDouble)

{-| __C declaration:__ @FLT4_3@

    __defined at:__ @macros\/macros.h:39:9@

    __exported by:__ @macros\/macros.h@
-}
fLT4_3 :: FC.CFloat
fLT4_3 = (6.7e-2 :: FC.CFloat)

{-| __C declaration:__ @FLT5_1@

    __defined at:__ @macros\/macros.h:41:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_1 :: FC.CDouble
fLT5_1 = (81.0 :: FC.CDouble)

{-| __C declaration:__ @FLT5_2@

    __defined at:__ @macros\/macros.h:42:9@

    __exported by:__ @macros\/macros.h@
-}
fLT5_2 :: FC.CFloat
fLT5_2 = (82.0 :: FC.CFloat)

{-| __C declaration:__ @FLT6_1@

    __defined at:__ @macros\/macros.h:44:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_1 :: FC.CDouble
fLT6_1 = (15520.0 :: FC.CDouble)

{-| __C declaration:__ @FLT6_2@

    __defined at:__ @macros\/macros.h:45:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_2 :: FC.CDouble
fLT6_2 = (98.0 :: FC.CDouble)

{-| __C declaration:__ @FLT6_3@

    __defined at:__ @macros\/macros.h:46:9@

    __exported by:__ @macros\/macros.h@
-}
fLT6_3 :: FC.CFloat
fLT6_3 = (9.9e-3 :: FC.CFloat)

{-| __C declaration:__ @BAD1@

    __defined at:__ @macros\/macros.h:49:9@

    __exported by:__ @macros\/macros.h@
-}
bAD1 :: FC.CDouble
bAD1 = (C.+) (0.1 :: FC.CDouble) (1 :: FC.CInt)

{-| __C declaration:__ @BAD2@

    __defined at:__ @macros\/macros.h:50:9@

    __exported by:__ @macros\/macros.h@
-}
bAD2 :: FC.CULong
bAD2 = (C.*) (2 :: FC.CLong) (2 :: FC.CULong)
