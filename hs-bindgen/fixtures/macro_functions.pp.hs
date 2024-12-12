{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Bits as DB
import qualified Foreign.C as FC
import qualified HsBindgen.Patterns as HsBindgen
import qualified Prelude as P

iNCR :: forall a0. P.Integral a0 => a0 -> a0
iNCR = \x0 -> (P.+) x0 1

aDD :: forall a0. P.Num a0 => a0 -> a0 -> a0
aDD = \x0 -> \y1 -> (P.+) x0 y1

iD :: forall a0. a0 -> a0
iD = \x0 -> x0

cONST :: forall a0 b1. a0 -> b1 -> a0
cONST = \x0 -> \y1 -> x0

cMP :: forall a0. P.Ord a0 => a0 -> a0 -> FC.CBool
cMP = \x0 -> \y1 -> (P.<) x0 y1

fUN1 :: FC.CULLong -> FC.CULLong -> FC.CULLong
fUN1 = \x0 -> \y1 -> (P.+) x0 ((P.*) 12 y1)

fUN2 :: forall a0. DB.Bits a0 => a0 -> FC.CULLong -> a0
fUN2 = \x0 -> \y1 -> DB.shiftL x0 ((P.*) 3 y1)

g :: forall a0 b1. P.Integral b1 => a0 -> b1 -> b1
g = \x0 -> \y1 -> cONST (iNCR y1) (iD x0)

dIV1 :: FC.CUInt -> FC.CUInt -> FC.CUInt
dIV1 = \x0 -> \y1 -> (HsBindgen./) x0 ((P.+) y1 12)

dIV2 :: FC.CFloat -> FC.CFloat -> FC.CFloat
dIV2 = \x0 -> \y1 -> (HsBindgen./) ((P.*) 10.0 x0) y1
