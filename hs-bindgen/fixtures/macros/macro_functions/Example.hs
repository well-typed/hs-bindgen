{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Example where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @INCR@

    __defined at:__ @macros\/macro_functions.h 1:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iNCR :: forall a0. (C.Expr.HostPlatform.Add a0) RIP.CInt => a0 -> (C.Expr.HostPlatform.AddRes a0) RIP.CInt
iNCR =
  \x0 -> (C.Expr.HostPlatform.+) x0 (1 :: RIP.CInt)

{-| __C declaration:__ @ADD@

    __defined at:__ @macros\/macro_functions.h 2:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aDD :: forall a0 b1. (C.Expr.HostPlatform.Add a0) b1 => a0 -> b1 -> (C.Expr.HostPlatform.AddRes a0) b1
aDD = \x0 -> \y1 -> (C.Expr.HostPlatform.+) x0 y1

{-| __C declaration:__ @ID@

    __defined at:__ @macros\/macro_functions.h 4:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @CONST@

    __defined at:__ @macros\/macro_functions.h 5:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST :: forall a0 b1. a0 -> b1 -> a0
cONST = \x0 -> \y1 -> x0

{-| __C declaration:__ @CMP@

    __defined at:__ @macros\/macro_functions.h 7:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cMP :: forall a0 b1. (C.Expr.HostPlatform.RelOrd a0) b1 => a0 -> b1 -> RIP.CInt
cMP = \x0 -> \y1 -> (C.Expr.HostPlatform.<) x0 y1

{-| __C declaration:__ @FUN1@

    __defined at:__ @macros\/macro_functions.h 8:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN1 :: forall a0 b1. (C.Expr.HostPlatform.Add a0) ((C.Expr.HostPlatform.MultRes RIP.CULLong) b1) => (C.Expr.HostPlatform.Mult RIP.CULLong) b1 => a0 -> b1 -> (C.Expr.HostPlatform.AddRes a0) ((C.Expr.HostPlatform.MultRes RIP.CULLong) b1)
fUN1 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform.+) x0 ((C.Expr.HostPlatform.*) (12 :: RIP.CULLong) y1)

{-| __C declaration:__ @FUN2@

    __defined at:__ @macros\/macro_functions.h 9:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN2 :: forall a0 b1. (C.Expr.HostPlatform.Mult RIP.CULLong) b1 => (C.Expr.HostPlatform.Shift a0) ((C.Expr.HostPlatform.MultRes RIP.CULLong) b1) => a0 -> b1 -> C.Expr.HostPlatform.ShiftRes a0
fUN2 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform.<<) x0 ((C.Expr.HostPlatform.*) (3 :: RIP.CULLong) y1)

{-| __C declaration:__ @G@

    __defined at:__ @macros\/macro_functions.h 11:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g :: forall a0 b1. (C.Expr.HostPlatform.Add a0) RIP.CInt => b1 -> a0 -> (C.Expr.HostPlatform.AddRes a0) RIP.CInt
g = \x0 -> \y1 -> cONST (iNCR y1) (iD x0)

{-| __C declaration:__ @DIV1@

    __defined at:__ @macros\/macro_functions.h 13:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV1 :: forall a0 b1. (C.Expr.HostPlatform.Add b1) RIP.CUInt => (C.Expr.HostPlatform.Div a0) ((C.Expr.HostPlatform.AddRes b1) RIP.CUInt) => a0 -> b1 -> (C.Expr.HostPlatform.DivRes a0) ((C.Expr.HostPlatform.AddRes b1) RIP.CUInt)
dIV1 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform./) x0 ((C.Expr.HostPlatform.+) y1 (12 :: RIP.CUInt))

{-| __C declaration:__ @DIV2@

    __defined at:__ @macros\/macro_functions.h 14:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV2 :: forall a0 b1. (C.Expr.HostPlatform.Mult RIP.CFloat) a0 => (C.Expr.HostPlatform.Div ((C.Expr.HostPlatform.MultRes RIP.CFloat) a0)) b1 => a0 -> b1 -> (C.Expr.HostPlatform.DivRes ((C.Expr.HostPlatform.MultRes RIP.CFloat) a0)) b1
dIV2 =
  \x0 ->
    \y1 ->
      (C.Expr.HostPlatform./) ((C.Expr.HostPlatform.*) (10.0 :: RIP.CFloat) x0) y1

{-| __C declaration:__ @SWAP32@

    __defined at:__ @macros\/macro_functions.h 18:9@

    __exported by:__ @macros\/macro_functions.h@
-}
sWAP32 :: forall a0. (C.Expr.HostPlatform.Bitwise ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) RIP.CInt)) ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) RIP.CInt) => (C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.ShiftRes a0)) RIP.CInt => (C.Expr.HostPlatform.Shift a0) RIP.CInt => a0 -> (C.Expr.HostPlatform.BitsRes ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) RIP.CInt)) ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) RIP.CInt)
sWAP32 =
  \w0 ->
    (C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform..&.) ((C.Expr.HostPlatform.>>) w0 (24 :: RIP.CInt)) (255 :: RIP.CInt)) ((C.Expr.HostPlatform..&.) ((C.Expr.HostPlatform.<<) w0 (8 :: RIP.CInt)) (16711680 :: RIP.CInt))

{-| __C declaration:__ @AV_VERSION_INT@

    __defined at:__ @macros\/macro_functions.h 19:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aV_VERSION_INT :: forall a0 b1 c2. (C.Expr.HostPlatform.Bitwise (C.Expr.HostPlatform.ShiftRes a0)) (C.Expr.HostPlatform.ShiftRes b1) => (C.Expr.HostPlatform.Bitwise ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) (C.Expr.HostPlatform.ShiftRes b1))) c2 => (C.Expr.HostPlatform.Shift b1) RIP.CInt => (C.Expr.HostPlatform.Shift a0) RIP.CInt => a0 -> b1 -> c2 -> (C.Expr.HostPlatform.BitsRes ((C.Expr.HostPlatform.BitsRes (C.Expr.HostPlatform.ShiftRes a0)) (C.Expr.HostPlatform.ShiftRes b1))) c2
aV_VERSION_INT =
  \a0 ->
    \b1 ->
      \c2 ->
        (C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform..|.) ((C.Expr.HostPlatform.<<) a0 (16 :: RIP.CInt)) ((C.Expr.HostPlatform.<<) b1 (8 :: RIP.CInt))) c2
