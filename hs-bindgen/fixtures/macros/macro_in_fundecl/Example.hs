{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified Prelude as P
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype C = C
  { un_C :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype F = F
  { un_F :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype L = L
  { un_L :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype S = S
  { un_S :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "wrapper" hs_bindgen_074b9de694d8f359_base ::
     (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32))

-- __unique:__ @instance ToFunPtr (FC.CShort -> IO I)@
hs_bindgen_074b9de694d8f359 ::
     (FC.CShort -> IO I)
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
hs_bindgen_074b9de694d8f359 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_074b9de694d8f359_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c7a8adce35e64925_base ::
     Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> GHC.Int.Int16 -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr (FC.CShort -> IO I)@
hs_bindgen_c7a8adce35e64925 ::
     Ptr.FunPtr (FC.CShort -> IO I)
  -> FC.CShort -> IO I
hs_bindgen_c7a8adce35e64925 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_c7a8adce35e64925_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr (FC.CShort -> IO I) where

  toFunPtr = hs_bindgen_074b9de694d8f359

instance HsBindgen.Runtime.FunPtr.FromFunPtr (FC.CShort -> IO I) where

  fromFunPtr = hs_bindgen_c7a8adce35e64925

foreign import ccall safe "wrapper" hs_bindgen_ffdbafa239adf14e_base ::
     (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32))

-- __unique:__ @instance ToFunPtr (S -> IO FC.CInt)@
hs_bindgen_ffdbafa239adf14e ::
     (S -> IO FC.CInt)
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))
hs_bindgen_ffdbafa239adf14e =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_ffdbafa239adf14e_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9c8a77fe3560cebd_base ::
     Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> GHC.Int.Int16 -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr (S -> IO FC.CInt)@
hs_bindgen_9c8a77fe3560cebd ::
     Ptr.FunPtr (S -> IO FC.CInt)
  -> S -> IO FC.CInt
hs_bindgen_9c8a77fe3560cebd =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_9c8a77fe3560cebd_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr (S -> IO FC.CInt) where

  toFunPtr = hs_bindgen_ffdbafa239adf14e

instance HsBindgen.Runtime.FunPtr.FromFunPtr (S -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_9c8a77fe3560cebd
