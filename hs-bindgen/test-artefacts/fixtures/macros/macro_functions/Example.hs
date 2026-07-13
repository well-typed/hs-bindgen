{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Example
    ( Example.iNCR
    , Example.aDD
    , Example.iD
    , Example.cONST
    , Example.cONST_3
    , Example.cMP
    , Example.fUN1
    , Example.fUN2
    , Example.g
    , Example.g_3
    , Example.dIV1
    , Example.dIV2
    , Example.sWAP32
    , Example.aV_VERSION_INT
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro INCR@

    __defined at:__ @macros\/macro_functions.h 1:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iNCR :: forall a0. C.Expr.HostPlatform.Add a0 BG.CInt => a0 -> C.Expr.HostPlatform.AddRes a0 BG.CInt
iNCR =
  \x0 -> (C.Expr.HostPlatform.+) x0 (1 :: BG.CInt)

{-| __C declaration:__ @macro ADD@

    __defined at:__ @macros\/macro_functions.h 2:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aDD :: forall a0 b1. C.Expr.HostPlatform.Add a0 b1 => a0 -> b1 -> C.Expr.HostPlatform.AddRes a0 b1
aDD = \x0 -> \y1 -> (C.Expr.HostPlatform.+) x0 y1

{-| __C declaration:__ @macro ID@

    __defined at:__ @macros\/macro_functions.h 4:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @macro CONST@

    __defined at:__ @macros\/macro_functions.h 5:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST :: forall a0 b1. a0 -> b1 -> a0
cONST = \x0 -> \y1 -> x0

{-| __C declaration:__ @macro CONST_3@

    __defined at:__ @macros\/macro_functions.h 6:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST_3 :: forall a0 b1 c2. a0 -> b1 -> c2 -> a0
cONST_3 = \x0 -> \y1 -> \z2 -> x0

{-| __C declaration:__ @macro CMP@

    __defined at:__ @macros\/macro_functions.h 8:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cMP :: forall a0 b1. C.Expr.HostPlatform.RelOrd a0 b1 => a0 -> b1 -> BG.CInt
cMP = \x0 -> \y1 -> (C.Expr.HostPlatform.<) x0 y1

{-| __C declaration:__ @macro FUN1@

    __defined at:__ @macros\/macro_functions.h 9:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN1 :: forall a0 b1. C.Expr.HostPlatform.Add a0 (C.Expr.HostPlatform.MultRes BG.CULLong b1) => C.Expr.HostPlatform.Mult BG.CULLong b1 => a0 -> b1 -> C.Expr.HostPlatform.AddRes a0 (C.Expr.HostPlatform.MultRes BG.CULLong b1)
fUN1 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform.+) x0 ((C.Expr.HostPlatform.*) (12 :: BG.CULLong) y1)

{-| __C declaration:__ @macro FUN2@

    __defined at:__ @macros\/macro_functions.h 10:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN2 :: forall a0 b1. C.Expr.HostPlatform.Mult BG.CULLong b1 => C.Expr.HostPlatform.Shift a0 (C.Expr.HostPlatform.MultRes BG.CULLong b1) => a0 -> b1 -> C.Expr.HostPlatform.ShiftRes a0
fUN2 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform.<<) x0 ((C.Expr.HostPlatform.*) (3 :: BG.CULLong) y1)

{-| __C declaration:__ @macro G@

    __defined at:__ @macros\/macro_functions.h 12:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g :: forall a0 b1. C.Expr.HostPlatform.Add b1 BG.CInt => a0 -> b1 -> C.Expr.HostPlatform.AddRes b1 BG.CInt
g = \x0 -> \y1 -> cONST (iNCR y1) (iD x0)

{-| __C declaration:__ @macro G_3@

    __defined at:__ @macros\/macro_functions.h 13:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g_3 :: forall a0 b1 c2. C.Expr.HostPlatform.Add b1 BG.CInt => a0 -> b1 -> c2 -> C.Expr.HostPlatform.AddRes b1 BG.CInt
g_3 =
  \x0 ->
    \y1 -> \z2 -> cONST_3 (iNCR y1) (iD x0) (iD z2)

{-| __C declaration:__ @macro DIV1@

    __defined at:__ @macros\/macro_functions.h 15:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV1 :: forall a0 b1. C.Expr.HostPlatform.Add b1 BG.CUInt => C.Expr.HostPlatform.Div a0 (C.Expr.HostPlatform.AddRes b1 BG.CUInt) => a0 -> b1 -> C.Expr.HostPlatform.DivRes a0 (C.Expr.HostPlatform.AddRes b1 BG.CUInt)
dIV1 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform./) x0 ((C.Expr.HostPlatform.+) y1 (12 :: BG.CUInt))

{-| __C declaration:__ @macro DIV2@

    __defined at:__ @macros\/macro_functions.h 16:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV2 :: forall a0 b1. C.Expr.HostPlatform.Mult BG.CFloat a0 => C.Expr.HostPlatform.Div (C.Expr.HostPlatform.MultRes BG.CFloat a0) b1 => a0 -> b1 -> C.Expr.HostPlatform.DivRes (C.Expr.HostPlatform.MultRes BG.CFloat a0) b1
dIV2 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform./) ((C.Expr.HostPlatform.*) (10.0 :: BG.CFloat) x0) y1

{-| __C declaration:__ @macro SWAP32@

    __defined at:__ @macros\/macro_functions.h 20:9@

    __exported by:__ @macros\/macro_functions.h@
-}
sWAP32 :: forall a0. C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) BG.CInt) (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) BG.CInt) => C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.ShiftRes a0) BG.CInt => C.Expr.HostPlatform.Shift a0 BG.CInt => a0 -> C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) BG.CInt) (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) BG.CInt)
sWAP32 =
  \w0 ->
    (C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform..&.) ((C.Expr.HostPlatform.>>) w0 (24 :: BG.CInt)) (255 :: BG.CInt)) ((C.Expr.HostPlatform..&.) ((C.Expr.HostPlatform.<<) w0 (8 :: BG.CInt)) (16711680 :: BG.CInt))

{-| __C declaration:__ @macro AV_VERSION_INT@

    __defined at:__ @macros\/macro_functions.h 21:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aV_VERSION_INT :: forall a0 b1 c2. C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.ShiftRes a0) (C.Expr.HostPlatform.ShiftRes b1) => C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) (C.Expr.HostPlatform.ShiftRes b1)) c2 => C.Expr.HostPlatform.Shift b1 BG.CInt => C.Expr.HostPlatform.Shift a0 BG.CInt => a0 -> b1 -> c2 -> C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0) (C.Expr.HostPlatform.ShiftRes b1)) c2
aV_VERSION_INT =
  \a0 ->
    \b1 ->
      \c2 ->
        (C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform.<<) a0 (16 :: BG.CInt)) ((C.Expr.HostPlatform.<<) b1 (8 :: BG.CInt))) c2
