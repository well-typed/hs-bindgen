{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import C.Typing ((*), (+), (/), (<), (<<))
import qualified C.Typing as C
import Data.Type.Equality ((~))
import qualified Foreign.C as FC
import qualified HsBindgen.Syntax as HsBindgen

iNCR :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
iNCR = \x0 -> (+) x0 1

aDD :: forall a0 b1. (C.Add a0) b1 => a0 -> b1 -> (C.AddRes a0) b1
aDD = \x0 -> \y1 -> (+) x0 y1

iD :: forall a0. a0 -> a0
iD = \x0 -> x0

cONST :: forall a0 b1. a0 -> b1 -> a0
cONST = \x0 -> \y1 -> x0

cMP :: forall a0 b1. (C.RelOrd a0) b1 => a0 -> b1 -> FC.CInt
cMP = \x0 -> \y1 -> (<) x0 y1

fUN1 :: forall a0 b1. (C.Add a0) ((C.MultRes FC.CULLong) b1) => (C.Mult FC.CULLong) b1 => a0 -> b1 -> (C.AddRes a0) ((C.MultRes FC.CULLong) b1)
fUN1 = \x0 -> \y1 -> (+) x0 ((*) 12 y1)

fUN2 :: forall a0 b1. (C.Mult FC.CULLong) b1 => ((~) (HsBindgen.IntLike b1)) ((C.MultRes FC.CULLong) b1) => (HsBindgen.IntLike a0) -> b1 -> C.ShiftRes (HsBindgen.IntLike a0)
fUN2 = \x0 -> \y1 -> (<<) x0 ((*) 3 y1)

g :: forall a0 b1. (C.Add b1) FC.CInt => a0 -> b1 -> (C.AddRes b1) FC.CInt
g = \x0 -> \y1 -> cONST (iNCR y1) (iD x0)

dIV1 :: forall a0 b1. (C.Add b1) FC.CUInt => (C.Div a0) ((C.AddRes b1) FC.CUInt) => a0 -> b1 -> (C.DivRes a0) ((C.AddRes b1) FC.CUInt)
dIV1 = \x0 -> \y1 -> (/) x0 ((+) y1 12)

dIV2 :: forall a0 b1. (C.Mult FC.CFloat) a0 => (C.Div ((C.MultRes FC.CFloat) a0)) b1 => a0 -> b1 -> (C.DivRes ((C.MultRes FC.CFloat) a0)) b1
dIV2 = \x0 -> \y1 -> (/) ((*) 10.0 x0) y1
