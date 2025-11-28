{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Expr.HostPlatform as C
import qualified Foreign.C as FC

{-| __C declaration:__ @INCR@

    __defined at:__ @macros\/macro_functions.h:1:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iNCR :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
iNCR = \x0 -> (C.+) x0 (1 :: FC.CInt)

{-| __C declaration:__ @ADD@

    __defined at:__ @macros\/macro_functions.h:2:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aDD :: forall a0 b1. (C.Add a0) b1 => a0 -> b1 -> (C.AddRes a0) b1
aDD = \x0 -> \y1 -> (C.+) x0 y1

{-| __C declaration:__ @ID@

    __defined at:__ @macros\/macro_functions.h:4:9@

    __exported by:__ @macros\/macro_functions.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @CONST@

    __defined at:__ @macros\/macro_functions.h:5:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cONST :: forall a0 b1. a0 -> b1 -> a0
cONST = \x0 -> \y1 -> x0

{-| __C declaration:__ @CMP@

    __defined at:__ @macros\/macro_functions.h:7:9@

    __exported by:__ @macros\/macro_functions.h@
-}
cMP :: forall a0 b1. (C.RelOrd a0) b1 => a0 -> b1 -> FC.CInt
cMP = \x0 -> \y1 -> (C.<) x0 y1

{-| __C declaration:__ @FUN1@

    __defined at:__ @macros\/macro_functions.h:8:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN1 :: forall a0 b1. (C.Add a0) ((C.MultRes FC.CULLong) b1) => (C.Mult FC.CULLong) b1 => a0 -> b1 -> (C.AddRes a0) ((C.MultRes FC.CULLong) b1)
fUN1 =
  \x0 -> \y1 -> (C.+) x0 ((C.*) (12 :: FC.CULLong) y1)

{-| __C declaration:__ @FUN2@

    __defined at:__ @macros\/macro_functions.h:9:9@

    __exported by:__ @macros\/macro_functions.h@
-}
fUN2 :: forall a0 b1. (C.Mult FC.CULLong) b1 => (C.Shift a0) ((C.MultRes FC.CULLong) b1) => a0 -> b1 -> C.ShiftRes a0
fUN2 =
  \x0 -> \y1 -> (C.<<) x0 ((C.*) (3 :: FC.CULLong) y1)

{-| __C declaration:__ @G@

    __defined at:__ @macros\/macro_functions.h:11:9@

    __exported by:__ @macros\/macro_functions.h@
-}
g :: forall a0 b1. (C.Add a0) FC.CInt => b1 -> a0 -> (C.AddRes a0) FC.CInt
g = \x0 -> \y1 -> cONST (iNCR y1) (iD x0)

{-| __C declaration:__ @DIV1@

    __defined at:__ @macros\/macro_functions.h:13:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV1 :: forall a0 b1. (C.Add b1) FC.CUInt => (C.Div a0) ((C.AddRes b1) FC.CUInt) => a0 -> b1 -> (C.DivRes a0) ((C.AddRes b1) FC.CUInt)
dIV1 =
  \x0 -> \y1 -> (C./) x0 ((C.+) y1 (12 :: FC.CUInt))

{-| __C declaration:__ @DIV2@

    __defined at:__ @macros\/macro_functions.h:14:9@

    __exported by:__ @macros\/macro_functions.h@
-}
dIV2 :: forall a0 b1. (C.Mult FC.CFloat) a0 => (C.Div ((C.MultRes FC.CFloat) a0)) b1 => a0 -> b1 -> (C.DivRes ((C.MultRes FC.CFloat) a0)) b1
dIV2 =
  \x0 -> \y1 -> (C./) ((C.*) (10.0 :: FC.CFloat) x0) y1

{-| __C declaration:__ @SWAP32@

    __defined at:__ @macros\/macro_functions.h:18:9@

    __exported by:__ @macros\/macro_functions.h@
-}
sWAP32 :: forall a0. (C.Bitwise ((C.BitsRes (C.ShiftRes a0)) FC.CInt)) ((C.BitsRes (C.ShiftRes a0)) FC.CInt) => (C.Bitwise (C.ShiftRes a0)) FC.CInt => (C.Shift a0) FC.CInt => a0 -> (C.BitsRes ((C.BitsRes (C.ShiftRes a0)) FC.CInt)) ((C.BitsRes (C.ShiftRes a0)) FC.CInt)
sWAP32 =
  \w0 ->
    (C..|.) ((C..&.) ((C.>>) w0 (24 :: FC.CInt)) (255 :: FC.CInt)) ((C..&.) ((C.<<) w0 (8 :: FC.CInt)) (16711680 :: FC.CInt))

{-| __C declaration:__ @AV_VERSION_INT@

    __defined at:__ @macros\/macro_functions.h:19:9@

    __exported by:__ @macros\/macro_functions.h@
-}
aV_VERSION_INT :: forall a0 b1 c2. (C.Bitwise (C.ShiftRes a0)) (C.ShiftRes b1) => (C.Bitwise ((C.BitsRes (C.ShiftRes a0)) (C.ShiftRes b1))) c2 => (C.Shift b1) FC.CInt => (C.Shift a0) FC.CInt => a0 -> b1 -> c2 -> (C.BitsRes ((C.BitsRes (C.ShiftRes a0)) (C.ShiftRes b1))) c2
aV_VERSION_INT =
  \a0 ->
    \b1 ->
      \c2 ->
        (C..|.) ((C..|.) ((C.<<) a0 (16 :: FC.CInt)) ((C.<<) b1 (8 :: FC.CInt))) c2
