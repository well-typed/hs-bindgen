{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Expr.HostPlatform as C
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Eq, Show)

{-| __C declaration:__ @N@

    __defined at:__ @type_naturals.h:4:9@

    __exported by:__ @type_naturals.h@
-}
n :: FC.CInt
n = (3 :: FC.CInt)

{-| __C declaration:__ @M@

    __defined at:__ @type_naturals.h:5:9@

    __exported by:__ @type_naturals.h@
-}
m :: FC.CInt
m = (C.+) (1 :: FC.CInt) n

{-| __C declaration:__ @F@

    __defined at:__ @type_naturals.h:6:9@

    __exported by:__ @type_naturals.h@
-}
f :: forall a0 b1. (C.Add a0) ((C.MultRes FC.CInt) b1) => (C.Sub ((C.AddRes a0) ((C.MultRes FC.CInt) b1))) FC.CInt => (C.Mult FC.CInt) b1 => a0 -> b1 -> (C.SubRes ((C.AddRes a0) ((C.MultRes FC.CInt) b1))) FC.CInt
f =
  \a0 ->
    \b1 ->
      (C.-) ((C.+) a0 ((C.*) (2 :: FC.CInt) b1)) (1 :: FC.CInt)

{-| __C declaration:__ @G@

    __defined at:__ @type_naturals.h:7:9@

    __exported by:__ @type_naturals.h@
-}
g :: forall a0 b1 c2. (C.Add ((C.MultRes FC.CInt) a0)) ((C.MultRes FC.CInt) b1) => (C.Mult FC.CInt) b1 => (C.Mult FC.CInt) a0 => c2 -> a0 -> b1 -> (C.AddRes ((C.MultRes FC.CInt) a0)) ((C.MultRes FC.CInt) b1)
g =
  \u0 ->
    \x1 ->
      \y2 ->
        (C.+) ((C.*) (10 :: FC.CInt) x1) ((C.*) (16 :: FC.CInt) y2)

{-| __C declaration:__ @K@

    __defined at:__ @type_naturals.h:8:9@

    __exported by:__ @type_naturals.h@
-}
k :: forall a0. (C.Add FC.CInt) ((C.MultRes FC.CInt) a0) => (C.Mult FC.CInt) a0 => a0 -> (C.AddRes FC.CInt) ((C.MultRes FC.CInt) a0)
k =
  g (11.77 :: FC.CDouble) (f (f (2 :: FC.CInt) m) n)

{-| __C declaration:__ @Arr1@

    __defined at:__ @type_naturals.h:10:9@

    __exported by:__ @type_naturals.h@
-}
newtype Arr1 = Arr1
  { un_Arr1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr2@

    __defined at:__ @type_naturals.h:11:9@

    __exported by:__ @type_naturals.h@
-}
newtype Arr2 = Arr2
  { un_Arr2 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr3@

    __defined at:__ @type_naturals.h:12:9@

    __exported by:__ @type_naturals.h@
-}
newtype Arr3 = Arr3
  { un_Arr3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 18) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr4@

    __defined at:__ @type_naturals.h:13:9@

    __exported by:__ @type_naturals.h@
-}
newtype Arr4 = Arr4
  { un_Arr4 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 252) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)
