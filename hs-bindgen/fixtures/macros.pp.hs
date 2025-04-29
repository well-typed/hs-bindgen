{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import C.Expr.HostPlatform ((*), (+))
import qualified Foreign.C as FC

oBJECTLIKE1 :: FC.CInt
oBJECTLIKE1 = (1 :: FC.CInt)

oBJECTLIKE2 :: FC.CInt
oBJECTLIKE2 = (2 :: FC.CInt)

oBJECTLIKE3 :: FC.CInt
oBJECTLIKE3 = (+) (3 :: FC.CInt) (3 :: FC.CInt)

oBJECTLIKE4 :: FC.CInt
oBJECTLIKE4 = (+) (4 :: FC.CInt) (4 :: FC.CInt)

mEANING_OF_LIFE1 :: FC.CInt
mEANING_OF_LIFE1 = (42 :: FC.CInt)

mEANING_OF_LIFE2 :: FC.CInt
mEANING_OF_LIFE2 = (42 :: FC.CInt)

mEANING_OF_LIFE3 :: FC.CInt
mEANING_OF_LIFE3 = (42 :: FC.CInt)

mEANING_OF_LIFE4 :: FC.CInt
mEANING_OF_LIFE4 = (42 :: FC.CInt)

mEANING_OF_LIFE5 :: FC.CInt
mEANING_OF_LIFE5 = (42 :: FC.CInt)

lONG_INT_TOKEN1 :: FC.CULLong
lONG_INT_TOKEN1 = (18446744073709550592 :: FC.CULLong)

lONG_INT_TOKEN2 :: FC.CULLong
lONG_INT_TOKEN2 = (18446744073709550592 :: FC.CULLong)

lONG_INT_TOKEN3 :: FC.CULLong
lONG_INT_TOKEN3 = (18446744073709550592 :: FC.CULLong)

lONG_INT_TOKEN4 :: FC.CULLong
lONG_INT_TOKEN4 = (18446744073709550592 :: FC.CULLong)

tUPLE1 :: ((,) FC.CInt) FC.CInt
tUPLE1 = (,) (1 :: FC.CInt) (2 :: FC.CInt)

tUPLE2 :: ((,) FC.CInt) FC.CInt
tUPLE2 = (,) (3 :: FC.CInt) (4 :: FC.CInt)

tUPLE3 :: ((,) FC.CInt) FC.CInt
tUPLE3 = (,) (5 :: FC.CInt) (6 :: FC.CInt)

fLT1_1 :: FC.CDouble
fLT1_1 = (110000.0 :: FC.CDouble)

fLT1_2 :: FC.CDouble
fLT1_2 = (1.2e-2 :: FC.CDouble)

fLT1_3 :: FC.CFloat
fLT1_3 = (1.3e-2 :: FC.CFloat)

fLT2_1 :: FC.CDouble
fLT2_1 = (21.0 :: FC.CDouble)

fLT2_2 :: FC.CDouble
fLT2_2 = (2200.0 :: FC.CDouble)

fLT2_3 :: FC.CFloat
fLT2_3 = (23.0 :: FC.CFloat)

fLT3_1 :: FC.CDouble
fLT3_1 = (31.0 :: FC.CDouble)

fLT3_2 :: FC.CDouble
fLT3_2 = (0.32 :: FC.CDouble)

fLT3_3 :: FC.CDouble
fLT3_3 = (33.0 :: FC.CDouble)

fLT3_4 :: FC.CFloat
fLT3_4 = (3.4e-3 :: FC.CFloat)

fLT4_1 :: FC.CDouble
fLT4_1 = (650000.0 :: FC.CDouble)

fLT4_2 :: FC.CDouble
fLT4_2 = (6.6e-2 :: FC.CDouble)

fLT4_3 :: FC.CFloat
fLT4_3 = (6.7e-2 :: FC.CFloat)

fLT5_1 :: FC.CDouble
fLT5_1 = (81.0 :: FC.CDouble)

fLT5_2 :: FC.CFloat
fLT5_2 = (82.0 :: FC.CFloat)

fLT6_1 :: FC.CDouble
fLT6_1 = (15520.0 :: FC.CDouble)

fLT6_2 :: FC.CDouble
fLT6_2 = (98.0 :: FC.CDouble)

fLT6_3 :: FC.CFloat
fLT6_3 = (9.9e-3 :: FC.CFloat)

bAD1 :: FC.CDouble
bAD1 = (+) (0.1 :: FC.CDouble) (1 :: FC.CInt)

bAD2 :: FC.CULong
bAD2 = (*) (2 :: FC.CLong) (2 :: FC.CULong)
