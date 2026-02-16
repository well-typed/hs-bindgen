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
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @myint@

    __defined at:__ @types\/typedefs\/typedefs.h 1:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Myint = Myint
  { unwrapMyint :: FC.CInt
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
         ) => GHC.Records.HasField "unwrapMyint" (Ptr.Ptr Myint) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyint")

instance HsBindgen.Runtime.HasCField.HasCField Myint "unwrapMyint" where

  type CFieldType Myint "unwrapMyint" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @intptr@

    __defined at:__ @types\/typedefs\/typedefs.h 2:15@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Intptr = Intptr
  { unwrapIntptr :: Ptr.Ptr FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.Ptr FC.CInt)
         ) => GHC.Records.HasField "unwrapIntptr" (Ptr.Ptr Intptr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapIntptr")

instance HsBindgen.Runtime.HasCField.HasCField Intptr "unwrapIntptr" where

  type CFieldType Intptr "unwrapIntptr" =
    Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int2int@

    __defined at:__ @types\/typedefs\/typedefs.h 5:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Int2int = Int2int
  { unwrapInt2int :: FC.CInt -> IO FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_a6c7dd49f5b9d470_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     Ptr.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( TyEq ty (FC.CInt -> IO FC.CInt)
         ) => GHC.Records.HasField "unwrapInt2int" (Ptr.Ptr Int2int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt2int")

instance HsBindgen.Runtime.HasCField.HasCField Int2int "unwrapInt2int" where

  type CFieldType Int2int "unwrapInt2int" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'FunctionPointer_Function'

__C declaration:__ @FunctionPointer_Function@

__defined at:__ @types\/typedefs\/typedefs.h 8:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function_Aux = FunctionPointer_Function_Aux
  { unwrapFunctionPointer_Function_Aux :: IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_b171c028cdc0781d_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toFunctionPointer_Function_Aux@
hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (Ptr.FunPtr FunctionPointer_Function_Aux)
hs_bindgen_b171c028cdc0781d =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_b171c028cdc0781d_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_4c3da8240a31e036_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFunctionPointer_Function_Aux@
hs_bindgen_4c3da8240a31e036 ::
     Ptr.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux
hs_bindgen_4c3da8240a31e036 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_4c3da8240a31e036_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr FunctionPointer_Function_Aux where

  toFunPtr = hs_bindgen_b171c028cdc0781d

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr FunctionPointer_Function_Aux where

  fromFunPtr = hs_bindgen_4c3da8240a31e036

instance ( TyEq ty (IO ())
         ) => GHC.Records.HasField "unwrapFunctionPointer_Function_Aux" (Ptr.Ptr FunctionPointer_Function_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunctionPointer_Function_Aux")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" where

  type CFieldType FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { unwrapFunctionPointer_Function :: Ptr.FunPtr FunctionPointer_Function_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr FunctionPointer_Function_Aux)
         ) => GHC.Records.HasField "unwrapFunctionPointer_Function" (Ptr.Ptr FunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function "unwrapFunctionPointer_Function" where

  type CFieldType FunctionPointer_Function "unwrapFunctionPointer_Function" =
    Ptr.FunPtr FunctionPointer_Function_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 9:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { unwrapNonFunctionPointer_Function :: FC.CInt -> IO FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_766ae751d60365e9_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toNonFunctionPointer_Function@
hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (Ptr.FunPtr NonFunctionPointer_Function)
hs_bindgen_766ae751d60365e9 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_766ae751d60365e9_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_36c7108d046bcbc3_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromNonFunctionPointer_Function@
hs_bindgen_36c7108d046bcbc3 ::
     Ptr.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
hs_bindgen_36c7108d046bcbc3 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_36c7108d046bcbc3_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = hs_bindgen_766ae751d60365e9

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = hs_bindgen_36c7108d046bcbc3

instance ( TyEq ty (FC.CInt -> IO FC.CInt)
         ) => GHC.Records.HasField "unwrapNonFunctionPointer_Function" (Ptr.Ptr NonFunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapNonFunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" where

  type CFieldType NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F1'

__C declaration:__ @f1@

__defined at:__ @types\/typedefs\/typedefs.h 11:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1_Aux = F1_Aux
  { unwrapF1_Aux :: IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (Ptr.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_00d16e666202ed6c_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     Ptr.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_ddeb5206e8192425_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr F1_Aux where

  toFunPtr = hs_bindgen_00d16e666202ed6c

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr F1_Aux where

  fromFunPtr = hs_bindgen_ddeb5206e8192425

instance ( TyEq ty (IO ())
         ) => GHC.Records.HasField "unwrapF1_Aux" (Ptr.Ptr F1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapF1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F1_Aux "unwrapF1_Aux" where

  type CFieldType F1_Aux "unwrapF1_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h 11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1 = F1
  { unwrapF1 :: Ptr.FunPtr F1_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr F1_Aux)
         ) => GHC.Records.HasField "unwrapF1" (Ptr.Ptr F1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapF1")

instance HsBindgen.Runtime.HasCField.HasCField F1 "unwrapF1" where

  type CFieldType F1 "unwrapF1" = Ptr.FunPtr F1_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g1@

    __defined at:__ @types\/typedefs\/typedefs.h 13:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G1 = G1
  { unwrapG1 :: IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_fa5806570b682579_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toG1@
hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (Ptr.FunPtr G1)
hs_bindgen_fa5806570b682579 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_fa5806570b682579_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8405c8e75aa78be5_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromG1@
hs_bindgen_8405c8e75aa78be5 ::
     Ptr.FunPtr G1
  -> G1
hs_bindgen_8405c8e75aa78be5 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_8405c8e75aa78be5_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr G1 where

  toFunPtr = hs_bindgen_fa5806570b682579

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr G1 where

  fromFunPtr = hs_bindgen_8405c8e75aa78be5

instance ( TyEq ty (IO ())
         ) => GHC.Records.HasField "unwrapG1" (Ptr.Ptr G1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapG1")

instance HsBindgen.Runtime.HasCField.HasCField G1 "unwrapG1" where

  type CFieldType G1 "unwrapG1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g2@

    __defined at:__ @types\/typedefs\/typedefs.h 14:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G2 = G2
  { unwrapG2 :: Ptr.FunPtr G1
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr G1)
         ) => GHC.Records.HasField "unwrapG2" (Ptr.Ptr G2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapG2")

instance HsBindgen.Runtime.HasCField.HasCField G2 "unwrapG2" where

  type CFieldType G2 "unwrapG2" = Ptr.FunPtr G1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h1@

    __defined at:__ @types\/typedefs\/typedefs.h 16:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H1 = H1
  { unwrapH1 :: IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_ffae0d1234ed018f_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toH1@
hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (Ptr.FunPtr H1)
hs_bindgen_ffae0d1234ed018f =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_ffae0d1234ed018f_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_1a33688324e1924f_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromH1@
hs_bindgen_1a33688324e1924f ::
     Ptr.FunPtr H1
  -> H1
hs_bindgen_1a33688324e1924f =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_1a33688324e1924f_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr H1 where

  toFunPtr = hs_bindgen_ffae0d1234ed018f

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr H1 where

  fromFunPtr = hs_bindgen_1a33688324e1924f

instance ( TyEq ty (IO ())
         ) => GHC.Records.HasField "unwrapH1" (Ptr.Ptr H1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapH1")

instance HsBindgen.Runtime.HasCField.HasCField H1 "unwrapH1" where

  type CFieldType H1 "unwrapH1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h2@

    __defined at:__ @types\/typedefs\/typedefs.h 17:12@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H2 = H2
  { unwrapH2 :: H1
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance ( TyEq ty H1
         ) => GHC.Records.HasField "unwrapH2" (Ptr.Ptr H2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapH2")

instance HsBindgen.Runtime.HasCField.HasCField H2 "unwrapH2" where

  type CFieldType H2 "unwrapH2" = H1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h3@

    __defined at:__ @types\/typedefs\/typedefs.h 18:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H3 = H3
  { unwrapH3 :: Ptr.FunPtr H2
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr H2)
         ) => GHC.Records.HasField "unwrapH3" (Ptr.Ptr H3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapH3")

instance HsBindgen.Runtime.HasCField.HasCField H3 "unwrapH3" where

  type CFieldType H3 "unwrapH3" = Ptr.FunPtr H2

  offset# = \_ -> \_ -> 0
