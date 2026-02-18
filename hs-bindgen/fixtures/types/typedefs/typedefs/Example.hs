{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @myint@

    __defined at:__ @types\/typedefs\/typedefs.h 1:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Myint = Myint
  { unwrapMyint :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapMyint" (RIP.Ptr Myint) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyint")

instance HasCField.HasCField Myint "unwrapMyint" where

  type CFieldType Myint "unwrapMyint" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @intptr@

    __defined at:__ @types\/typedefs\/typedefs.h 2:15@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Intptr = Intptr
  { unwrapIntptr :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "unwrapIntptr" (RIP.Ptr Intptr) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapIntptr")

instance HasCField.HasCField Intptr "unwrapIntptr" where

  type CFieldType Intptr "unwrapIntptr" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int2int@

    __defined at:__ @types\/typedefs\/typedefs.h 5:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Int2int = Int2int
  { unwrapInt2int :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (RIP.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_a6c7dd49f5b9d470_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     RIP.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance RIP.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapInt2int" (RIP.Ptr Int2int) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt2int")

instance HasCField.HasCField Int2int "unwrapInt2int" where

  type CFieldType Int2int "unwrapInt2int" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'FunctionPointer_Function'

__C declaration:__ @FunctionPointer_Function@

__defined at:__ @types\/typedefs\/typedefs.h 8:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function_Aux = FunctionPointer_Function_Aux
  { unwrapFunctionPointer_Function_Aux :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_b171c028cdc0781d_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toFunctionPointer_Function_Aux@
hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (RIP.FunPtr FunctionPointer_Function_Aux)
hs_bindgen_b171c028cdc0781d =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b171c028cdc0781d_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_4c3da8240a31e036_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFunctionPointer_Function_Aux@
hs_bindgen_4c3da8240a31e036 ::
     RIP.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux
hs_bindgen_4c3da8240a31e036 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_4c3da8240a31e036_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr FunctionPointer_Function_Aux where

  toFunPtr = hs_bindgen_b171c028cdc0781d

instance RIP.FromFunPtr FunctionPointer_Function_Aux where

  fromFunPtr = hs_bindgen_4c3da8240a31e036

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapFunctionPointer_Function_Aux" (RIP.Ptr FunctionPointer_Function_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunctionPointer_Function_Aux")

instance HasCField.HasCField FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" where

  type CFieldType FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { unwrapFunctionPointer_Function :: RIP.FunPtr FunctionPointer_Function_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr FunctionPointer_Function_Aux)
         ) => RIP.HasField "unwrapFunctionPointer_Function" (RIP.Ptr FunctionPointer_Function) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunctionPointer_Function")

instance HasCField.HasCField FunctionPointer_Function "unwrapFunctionPointer_Function" where

  type CFieldType FunctionPointer_Function "unwrapFunctionPointer_Function" =
    RIP.FunPtr FunctionPointer_Function_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 9:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { unwrapNonFunctionPointer_Function :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_766ae751d60365e9_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toNonFunctionPointer_Function@
hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (RIP.FunPtr NonFunctionPointer_Function)
hs_bindgen_766ae751d60365e9 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_766ae751d60365e9_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_36c7108d046bcbc3_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromNonFunctionPointer_Function@
hs_bindgen_36c7108d046bcbc3 ::
     RIP.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
hs_bindgen_36c7108d046bcbc3 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_36c7108d046bcbc3_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = hs_bindgen_766ae751d60365e9

instance RIP.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = hs_bindgen_36c7108d046bcbc3

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapNonFunctionPointer_Function" (RIP.Ptr NonFunctionPointer_Function) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapNonFunctionPointer_Function")

instance HasCField.HasCField NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" where

  type CFieldType NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F1'

__C declaration:__ @f1@

__defined at:__ @types\/typedefs\/typedefs.h 11:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1_Aux = F1_Aux
  { unwrapF1_Aux :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (RIP.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_00d16e666202ed6c_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     RIP.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_ddeb5206e8192425_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F1_Aux where

  toFunPtr = hs_bindgen_00d16e666202ed6c

instance RIP.FromFunPtr F1_Aux where

  fromFunPtr = hs_bindgen_ddeb5206e8192425

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapF1_Aux" (RIP.Ptr F1_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF1_Aux")

instance HasCField.HasCField F1_Aux "unwrapF1_Aux" where

  type CFieldType F1_Aux "unwrapF1_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h 11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1 = F1
  { unwrapF1 :: RIP.FunPtr F1_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr F1_Aux)
         ) => RIP.HasField "unwrapF1" (RIP.Ptr F1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF1")

instance HasCField.HasCField F1 "unwrapF1" where

  type CFieldType F1 "unwrapF1" = RIP.FunPtr F1_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g1@

    __defined at:__ @types\/typedefs\/typedefs.h 13:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G1 = G1
  { unwrapG1 :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_fa5806570b682579_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toG1@
hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (RIP.FunPtr G1)
hs_bindgen_fa5806570b682579 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_fa5806570b682579_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8405c8e75aa78be5_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromG1@
hs_bindgen_8405c8e75aa78be5 ::
     RIP.FunPtr G1
  -> G1
hs_bindgen_8405c8e75aa78be5 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_8405c8e75aa78be5_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr G1 where

  toFunPtr = hs_bindgen_fa5806570b682579

instance RIP.FromFunPtr G1 where

  fromFunPtr = hs_bindgen_8405c8e75aa78be5

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapG1" (RIP.Ptr G1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapG1")

instance HasCField.HasCField G1 "unwrapG1" where

  type CFieldType G1 "unwrapG1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g2@

    __defined at:__ @types\/typedefs\/typedefs.h 14:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G2 = G2
  { unwrapG2 :: RIP.FunPtr G1
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr G1)
         ) => RIP.HasField "unwrapG2" (RIP.Ptr G2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapG2")

instance HasCField.HasCField G2 "unwrapG2" where

  type CFieldType G2 "unwrapG2" = RIP.FunPtr G1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h1@

    __defined at:__ @types\/typedefs\/typedefs.h 16:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H1 = H1
  { unwrapH1 :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_ffae0d1234ed018f_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toH1@
hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (RIP.FunPtr H1)
hs_bindgen_ffae0d1234ed018f =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_ffae0d1234ed018f_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_1a33688324e1924f_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromH1@
hs_bindgen_1a33688324e1924f ::
     RIP.FunPtr H1
  -> H1
hs_bindgen_1a33688324e1924f =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_1a33688324e1924f_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr H1 where

  toFunPtr = hs_bindgen_ffae0d1234ed018f

instance RIP.FromFunPtr H1 where

  fromFunPtr = hs_bindgen_1a33688324e1924f

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapH1" (RIP.Ptr H1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapH1")

instance HasCField.HasCField H1 "unwrapH1" where

  type CFieldType H1 "unwrapH1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h2@

    __defined at:__ @types\/typedefs\/typedefs.h 17:12@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H2 = H2
  { unwrapH2 :: H1
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ((~) ty) H1
         ) => RIP.HasField "unwrapH2" (RIP.Ptr H2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapH2")

instance HasCField.HasCField H2 "unwrapH2" where

  type CFieldType H2 "unwrapH2" = H1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h3@

    __defined at:__ @types\/typedefs\/typedefs.h 18:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H3 = H3
  { unwrapH3 :: RIP.FunPtr H2
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr H2)
         ) => RIP.HasField "unwrapH3" (RIP.Ptr H3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapH3")

instance HasCField.HasCField H3 "unwrapH3" where

  type CFieldType H3 "unwrapH3" = RIP.FunPtr H2

  offset# = \_ -> \_ -> 0
