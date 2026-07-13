{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Myint(..)
    , Example.Intptr(..)
    , Example.Int2int(..)
    , Example.FunctionPointer_Function_Aux(..)
    , Example.FunctionPointer_Function(..)
    , Example.NonFunctionPointer_Function(..)
    , Example.F1_Aux(..)
    , Example.F1(..)
    , Example.G1(..)
    , Example.G2(..)
    , Example.H1(..)
    , Example.H2(..)
    , Example.H3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @myint@

    __defined at:__ @types\/typedefs\/typedefs.h 1:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Myint = Myint
  { unwrapMyint :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapMyint" Myint ty where

  hasField =
    \x0 ->
      (\y1 ->
         Myint {unwrapMyint = y1}, BG.getField @"unwrapMyint" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMyint" (BG.Ptr Myint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyint")

instance HasCField.HasCField Myint "unwrapMyint" where

  type CFieldType Myint "unwrapMyint" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @intptr@

    __defined at:__ @types\/typedefs\/typedefs.h 2:15@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Intptr = Intptr
  { unwrapIntptr :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapIntptr" Intptr ty where

  hasField =
    \x0 ->
      (\y1 ->
         Intptr {unwrapIntptr = y1}, BG.getField @"unwrapIntptr" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapIntptr" (BG.Ptr Intptr) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapIntptr")

instance HasCField.HasCField Intptr "unwrapIntptr" where

  type CFieldType Intptr "unwrapIntptr" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int2int@

    __defined at:__ @types\/typedefs\/typedefs.h 5:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Int2int = Int2int
  { unwrapInt2int :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toInt2int@
foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (BG.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_a6c7dd49f5b9d470_base (BG.toFFIType fun0))

-- __unique:__ @fromInt2int@
foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     BG.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance BG.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapInt2int" Int2int ty where

  hasField =
    \x0 ->
      (\y1 ->
         Int2int {unwrapInt2int = y1}, BG.getField @"unwrapInt2int" x0)

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapInt2int" (BG.Ptr Int2int) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt2int")

instance HasCField.HasCField Int2int "unwrapInt2int" where

  type CFieldType Int2int "unwrapInt2int" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'FunctionPointer_Function'

    __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function_Aux = FunctionPointer_Function_Aux
  { unwrapFunctionPointer_Function_Aux :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunctionPointer_Function_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b171c028cdc0781d_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toFunctionPointer_Function_Aux@
hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (BG.FunPtr FunctionPointer_Function_Aux)
hs_bindgen_b171c028cdc0781d =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_b171c028cdc0781d_base (BG.toFFIType fun0))

-- __unique:__ @fromFunctionPointer_Function_Aux@
foreign import ccall safe "dynamic" hs_bindgen_4c3da8240a31e036_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFunctionPointer_Function_Aux@
hs_bindgen_4c3da8240a31e036 ::
     BG.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux
hs_bindgen_4c3da8240a31e036 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_4c3da8240a31e036_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr FunctionPointer_Function_Aux where

  toFunPtr = hs_bindgen_b171c028cdc0781d

instance BG.FromFunPtr FunctionPointer_Function_Aux where

  fromFunPtr = hs_bindgen_4c3da8240a31e036

instance ( ty ~ IO ()
         ) => BG.CompatHasField.HasField "unwrapFunctionPointer_Function_Aux" FunctionPointer_Function_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          FunctionPointer_Function_Aux {unwrapFunctionPointer_Function_Aux = y1}
      , BG.getField @"unwrapFunctionPointer_Function_Aux" x0
      )

instance ( ty ~ IO ()
         ) => BG.HasField "unwrapFunctionPointer_Function_Aux" (BG.Ptr FunctionPointer_Function_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunctionPointer_Function_Aux")

instance HasCField.HasCField FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" where

  type CFieldType FunctionPointer_Function_Aux "unwrapFunctionPointer_Function_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { unwrapFunctionPointer_Function :: BG.FunPtr FunctionPointer_Function_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr FunctionPointer_Function_Aux
         ) => BG.CompatHasField.HasField "unwrapFunctionPointer_Function" FunctionPointer_Function ty where

  hasField =
    \x0 ->
      ( \y1 ->
          FunctionPointer_Function {unwrapFunctionPointer_Function = y1}
      , BG.getField @"unwrapFunctionPointer_Function" x0
      )

instance ( ty ~ BG.FunPtr FunctionPointer_Function_Aux
         ) => BG.HasField "unwrapFunctionPointer_Function" (BG.Ptr FunctionPointer_Function) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunctionPointer_Function")

instance HasCField.HasCField FunctionPointer_Function "unwrapFunctionPointer_Function" where

  type CFieldType FunctionPointer_Function "unwrapFunctionPointer_Function" =
    BG.FunPtr FunctionPointer_Function_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 9:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { unwrapNonFunctionPointer_Function :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toNonFunctionPointer_Function@
foreign import ccall safe "wrapper" hs_bindgen_766ae751d60365e9_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toNonFunctionPointer_Function@
hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (BG.FunPtr NonFunctionPointer_Function)
hs_bindgen_766ae751d60365e9 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_766ae751d60365e9_base (BG.toFFIType fun0))

-- __unique:__ @fromNonFunctionPointer_Function@
foreign import ccall safe "dynamic" hs_bindgen_36c7108d046bcbc3_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromNonFunctionPointer_Function@
hs_bindgen_36c7108d046bcbc3 ::
     BG.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
hs_bindgen_36c7108d046bcbc3 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_36c7108d046bcbc3_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = hs_bindgen_766ae751d60365e9

instance BG.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = hs_bindgen_36c7108d046bcbc3

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapNonFunctionPointer_Function" NonFunctionPointer_Function ty where

  hasField =
    \x0 ->
      ( \y1 ->
          NonFunctionPointer_Function {unwrapNonFunctionPointer_Function = y1}
      , BG.getField @"unwrapNonFunctionPointer_Function" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapNonFunctionPointer_Function" (BG.Ptr NonFunctionPointer_Function) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapNonFunctionPointer_Function")

instance HasCField.HasCField NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" where

  type CFieldType NonFunctionPointer_Function "unwrapNonFunctionPointer_Function" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F1'

    __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h 11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1_Aux = F1_Aux
  { unwrapF1_Aux :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_00d16e666202ed6c_base (BG.toFFIType fun0))

-- __unique:__ @fromF1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     BG.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_ddeb5206e8192425_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F1_Aux where

  toFunPtr = hs_bindgen_00d16e666202ed6c

instance BG.FromFunPtr F1_Aux where

  fromFunPtr = hs_bindgen_ddeb5206e8192425

instance ( ty ~ IO ()
         ) => BG.CompatHasField.HasField "unwrapF1_Aux" F1_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F1_Aux {unwrapF1_Aux = y1}, BG.getField @"unwrapF1_Aux" x0)

instance ( ty ~ IO ()
         ) => BG.HasField "unwrapF1_Aux" (BG.Ptr F1_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF1_Aux")

instance HasCField.HasCField F1_Aux "unwrapF1_Aux" where

  type CFieldType F1_Aux "unwrapF1_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h 11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1 = F1
  { unwrapF1 :: BG.FunPtr F1_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr F1_Aux
         ) => BG.CompatHasField.HasField "unwrapF1" F1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F1 {unwrapF1 = y1}, BG.getField @"unwrapF1" x0)

instance ( ty ~ BG.FunPtr F1_Aux
         ) => BG.HasField "unwrapF1" (BG.Ptr F1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF1")

instance HasCField.HasCField F1 "unwrapF1" where

  type CFieldType F1 "unwrapF1" = BG.FunPtr F1_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g1@

    __defined at:__ @types\/typedefs\/typedefs.h 13:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G1 = G1
  { unwrapG1 :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toG1@
foreign import ccall safe "wrapper" hs_bindgen_fa5806570b682579_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toG1@
hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (BG.FunPtr G1)
hs_bindgen_fa5806570b682579 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_fa5806570b682579_base (BG.toFFIType fun0))

-- __unique:__ @fromG1@
foreign import ccall safe "dynamic" hs_bindgen_8405c8e75aa78be5_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromG1@
hs_bindgen_8405c8e75aa78be5 ::
     BG.FunPtr G1
  -> G1
hs_bindgen_8405c8e75aa78be5 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_8405c8e75aa78be5_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr G1 where

  toFunPtr = hs_bindgen_fa5806570b682579

instance BG.FromFunPtr G1 where

  fromFunPtr = hs_bindgen_8405c8e75aa78be5

instance (ty ~ IO ()) => BG.CompatHasField.HasField "unwrapG1" G1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         G1 {unwrapG1 = y1}, BG.getField @"unwrapG1" x0)

instance (ty ~ IO ()) => BG.HasField "unwrapG1" (BG.Ptr G1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapG1")

instance HasCField.HasCField G1 "unwrapG1" where

  type CFieldType G1 "unwrapG1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g2@

    __defined at:__ @types\/typedefs\/typedefs.h 14:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G2 = G2
  { unwrapG2 :: BG.FunPtr G1
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr G1
         ) => BG.CompatHasField.HasField "unwrapG2" G2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         G2 {unwrapG2 = y1}, BG.getField @"unwrapG2" x0)

instance ( ty ~ BG.FunPtr G1
         ) => BG.HasField "unwrapG2" (BG.Ptr G2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapG2")

instance HasCField.HasCField G2 "unwrapG2" where

  type CFieldType G2 "unwrapG2" = BG.FunPtr G1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h1@

    __defined at:__ @types\/typedefs\/typedefs.h 16:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H1 = H1
  { unwrapH1 :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toH1@
foreign import ccall safe "wrapper" hs_bindgen_ffae0d1234ed018f_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toH1@
hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (BG.FunPtr H1)
hs_bindgen_ffae0d1234ed018f =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_ffae0d1234ed018f_base (BG.toFFIType fun0))

-- __unique:__ @fromH1@
foreign import ccall safe "dynamic" hs_bindgen_1a33688324e1924f_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromH1@
hs_bindgen_1a33688324e1924f ::
     BG.FunPtr H1
  -> H1
hs_bindgen_1a33688324e1924f =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_1a33688324e1924f_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr H1 where

  toFunPtr = hs_bindgen_ffae0d1234ed018f

instance BG.FromFunPtr H1 where

  fromFunPtr = hs_bindgen_1a33688324e1924f

instance (ty ~ IO ()) => BG.CompatHasField.HasField "unwrapH1" H1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         H1 {unwrapH1 = y1}, BG.getField @"unwrapH1" x0)

instance (ty ~ IO ()) => BG.HasField "unwrapH1" (BG.Ptr H1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapH1")

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
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance (ty ~ H1) => BG.CompatHasField.HasField "unwrapH2" H2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         H2 {unwrapH2 = y1}, BG.getField @"unwrapH2" x0)

instance (ty ~ H1) => BG.HasField "unwrapH2" (BG.Ptr H2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapH2")

instance HasCField.HasCField H2 "unwrapH2" where

  type CFieldType H2 "unwrapH2" = H1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h3@

    __defined at:__ @types\/typedefs\/typedefs.h 18:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H3 = H3
  { unwrapH3 :: BG.FunPtr H2
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr H2
         ) => BG.CompatHasField.HasField "unwrapH3" H3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         H3 {unwrapH3 = y1}, BG.getField @"unwrapH3" x0)

instance ( ty ~ BG.FunPtr H2
         ) => BG.HasField "unwrapH3" (BG.Ptr H3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapH3")

instance HasCField.HasCField H3 "unwrapH3" where

  type CFieldType H3 "unwrapH3" = BG.FunPtr H2

  offset# = \_ -> \_ -> 0
