{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype I = I
  { unwrapI :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapI" (Ptr.Ptr I) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapI")

instance HsBindgen.Runtime.HasCField.HasCField I "unwrapI" where

  type CFieldType I "unwrapI" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype C = C
  { unwrapC :: FC.CChar
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "unwrapC" (Ptr.Ptr C) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapC")

instance HsBindgen.Runtime.HasCField.HasCField C "unwrapC" where

  type CFieldType C "unwrapC" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype F = F
  { unwrapF :: FC.CFloat
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , Enum
    , Floating
    , Fractional
    , Num
    , Real
    , RealFloat
    , RealFrac
    )

instance ( TyEq ty FC.CFloat
         ) => GHC.Records.HasField "unwrapF" (Ptr.Ptr F) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapF")

instance HsBindgen.Runtime.HasCField.HasCField F "unwrapF" where

  type CFieldType F "unwrapF" = FC.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype L = L
  { unwrapL :: FC.CLong
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CLong
         ) => GHC.Records.HasField "unwrapL" (Ptr.Ptr L) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapL")

instance HsBindgen.Runtime.HasCField.HasCField L "unwrapL" where

  type CFieldType L "unwrapL" = FC.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype S = S
  { unwrapS :: FC.CShort
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CShort
         ) => GHC.Records.HasField "unwrapS" (Ptr.Ptr S) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapS")

instance HsBindgen.Runtime.HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" = FC.CShort

  offset# = \_ -> \_ -> 0

foreign import ccall safe "wrapper" hs_bindgen_074b9de694d8f359_base ::
     (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32))

-- __unique:__ @instance ToFunPtr (FC.CShort -> IO I)@
hs_bindgen_074b9de694d8f359 ::
     (FC.CShort -> IO I)
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
hs_bindgen_074b9de694d8f359 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_074b9de694d8f359_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c7a8adce35e64925_base ::
     Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> GHC.Int.Int16 -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr (FC.CShort -> IO I)@
hs_bindgen_c7a8adce35e64925 ::
     Ptr.FunPtr (FC.CShort -> IO I)
  -> FC.CShort -> IO I
hs_bindgen_c7a8adce35e64925 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_c7a8adce35e64925_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr (FC.CShort -> IO I) where

  toFunPtr = hs_bindgen_074b9de694d8f359

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr (FC.CShort -> IO I) where

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
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_ffdbafa239adf14e_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9c8a77fe3560cebd_base ::
     Ptr.FunPtr (GHC.Int.Int16 -> IO GHC.Int.Int32)
  -> GHC.Int.Int16 -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr (S -> IO FC.CInt)@
hs_bindgen_9c8a77fe3560cebd ::
     Ptr.FunPtr (S -> IO FC.CInt)
  -> S -> IO FC.CInt
hs_bindgen_9c8a77fe3560cebd =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_9c8a77fe3560cebd_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr (S -> IO FC.CInt) where

  toFunPtr = hs_bindgen_ffdbafa239adf14e

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr (S -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_9c8a77fe3560cebd
