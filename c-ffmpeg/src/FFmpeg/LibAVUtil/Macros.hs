{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FFmpeg.LibAVUtil.Macros where

import C.Expr.HostPlatform ((+), (-), (.&.), (<), (>))
import qualified C.Expr.HostPlatform as C
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Syntax as HsBindgen
import Prelude (Int, (~))

aV_NE :: forall a0 b1. b1 -> a0 -> a0
aV_NE = \be0 -> \le1 -> le1

fFDIFFSIGN :: forall a0 b1. (C.RelOrd a0) b1 => a0 -> b1 -> FC.CInt
fFDIFFSIGN =
  \x0 -> \y1 -> (-) ((>) x0 y1) ((<) x0 y1)

aV_JOIN :: (((,) (F.Ptr FC.CChar)) Int) -> (((,) (F.Ptr FC.CChar)) Int) -> ((,) (F.Ptr FC.CChar)) Int
aV_JOIN = \a0 -> \b1 -> aV_GLUE a0 b1

fFALIGN :: forall a0 b1. (C.Add a0) b1 => (C.Sub b1) FC.CInt => (C.Sub ((C.AddRes a0) b1)) FC.CInt => ((~) (HsBindgen.IntLike b1)) ((C.SubRes ((C.AddRes a0) b1)) FC.CInt) => ((~) (HsBindgen.IntLike b1)) (C.ComplementRes ((C.SubRes b1) FC.CInt)) => ((~) FC.CInt) ((C.SubRes b1) FC.CInt) => a0 -> b1 -> (C.BitsRes ((C.SubRes ((C.AddRes a0) b1)) FC.CInt)) (C.ComplementRes ((C.SubRes b1) FC.CInt))
fFALIGN =
  \x0 ->
    \a1 ->
      (.&.) ((-) ((+) x0 a1) (1 :: FC.CInt)) ((C..~) ((-) a1 (1 :: FC.CInt)))
