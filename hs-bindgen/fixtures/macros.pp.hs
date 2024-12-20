{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign.C as FC
import qualified Prelude as P

oBJECTLIKE1 :: forall a0. P.Integral a0 => a0
oBJECTLIKE1 = 1

oBJECTLIKE2 :: forall a0. P.Integral a0 => a0
oBJECTLIKE2 = 2

oBJECTLIKE3 :: forall a0. P.Integral a0 => a0
oBJECTLIKE3 = (P.+) 3 3

oBJECTLIKE4 :: forall a0. P.Integral a0 => a0
oBJECTLIKE4 = (P.+) 4 4

mEANING_OF_LIFE1 :: forall a0. P.Integral a0 => a0
mEANING_OF_LIFE1 = 42

mEANING_OF_LIFE2 :: forall a0. P.Integral a0 => a0
mEANING_OF_LIFE2 = 42

mEANING_OF_LIFE3 :: forall a0. P.Integral a0 => a0
mEANING_OF_LIFE3 = 42

mEANING_OF_LIFE4 :: forall a0. P.Integral a0 => a0
mEANING_OF_LIFE4 = 42

mEANING_OF_LIFE5 :: forall a0. P.Integral a0 => a0
mEANING_OF_LIFE5 = 42

lONG_INT_TOKEN1 :: FC.CULLong
lONG_INT_TOKEN1 = 18446744073709550592

lONG_INT_TOKEN2 :: FC.CULLong
lONG_INT_TOKEN2 = 18446744073709550592

lONG_INT_TOKEN3 :: FC.CULLong
lONG_INT_TOKEN3 = 18446744073709550592

lONG_INT_TOKEN4 :: FC.CULLong
lONG_INT_TOKEN4 = 18446744073709550592

fLT1_1 :: forall a0. P.Fractional a0 => a0
fLT1_1 = 110000.0

fLT1_2 :: forall a0. P.Fractional a0 => a0
fLT1_2 = 1.2e-2

fLT1_3 :: FC.CFloat
fLT1_3 = 1.3e-2

fLT2_1 :: forall a0. P.Fractional a0 => a0
fLT2_1 = 21.0

fLT2_2 :: forall a0. P.Fractional a0 => a0
fLT2_2 = 2200.0

fLT2_3 :: FC.CFloat
fLT2_3 = 23.0

fLT3_1 :: forall a0. P.Fractional a0 => a0
fLT3_1 = 31.0

fLT3_2 :: forall a0. P.Fractional a0 => a0
fLT3_2 = 0.32

fLT3_3 :: forall a0. P.Fractional a0 => a0
fLT3_3 = 33.0

fLT3_4 :: FC.CFloat
fLT3_4 = 3.4e-3

fLT4_1 :: forall a0. P.Fractional a0 => a0
fLT4_1 = 650000.0

fLT4_2 :: forall a0. P.Fractional a0 => a0
fLT4_2 = 6.6e-2

fLT4_3 :: FC.CFloat
fLT4_3 = 6.7e-2

fLT5_1 :: forall a0. P.Fractional a0 => a0
fLT5_1 = 81.0

fLT5_2 :: FC.CFloat
fLT5_2 = 82.0

fLT6_1 :: forall a0. P.Fractional a0 => a0
fLT6_1 = 15520.0

fLT6_2 :: forall a0. P.Fractional a0 => a0
fLT6_2 = 98.0

fLT6_3 :: FC.CFloat
fLT6_3 = 9.9e-3
