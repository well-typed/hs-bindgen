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
    ( Example.F1_Aux(..)
    , Example.F1(..)
    , Example.F2_Aux(..)
    , Example.F2(..)
    , Example.F3_Aux(..)
    , Example.F3(..)
    , Example.F4_Aux(..)
    , Example.F4(..)
    , Example.F5_Aux(..)
    , Example.F5(..)
    , Example.MyInt(..)
    , Example.F6_Aux(..)
    , Example.F6(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'F1'

    __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 7:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F1_Aux = F1_Aux
  { unwrapF1_Aux :: BG.CInt -> BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     (BG.Int32 -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_00d16e666202ed6c_base (BG.toFFIType fun0))

-- __unique:__ @fromF1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ())
  -> BG.Int32 -> BG.Int32 -> IO ()

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

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapF1_Aux" F1_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F1_Aux {unwrapF1_Aux = y1}, BG.getField @"unwrapF1_Aux" x0)

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.HasField "unwrapF1_Aux" (BG.Ptr F1_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF1_Aux")

instance HasCField.HasCField F1_Aux "unwrapF1_Aux" where

  type CFieldType F1_Aux "unwrapF1_Aux" =
    BG.CInt -> BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 7:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
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

{-| Auxiliary type used by 'F2'

    __C declaration:__ @f2@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 10:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F2_Aux = F2_Aux
  { unwrapF2_Aux :: BG.CInt -> BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c39d7524b75b54e8_base ::
     (BG.Int32 -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ()))

-- __unique:__ @toF2_Aux@
hs_bindgen_c39d7524b75b54e8 ::
     F2_Aux
  -> IO (BG.FunPtr F2_Aux)
hs_bindgen_c39d7524b75b54e8 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_c39d7524b75b54e8_base (BG.toFFIType fun0))

-- __unique:__ @fromF2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_e15bcd26f1ed1df7_base ::
     BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ())
  -> BG.Int32 -> BG.Int32 -> IO ()

-- __unique:__ @fromF2_Aux@
hs_bindgen_e15bcd26f1ed1df7 ::
     BG.FunPtr F2_Aux
  -> F2_Aux
hs_bindgen_e15bcd26f1ed1df7 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_e15bcd26f1ed1df7_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F2_Aux where

  toFunPtr = hs_bindgen_c39d7524b75b54e8

instance BG.FromFunPtr F2_Aux where

  fromFunPtr = hs_bindgen_e15bcd26f1ed1df7

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapF2_Aux" F2_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F2_Aux {unwrapF2_Aux = y1}, BG.getField @"unwrapF2_Aux" x0)

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.HasField "unwrapF2_Aux" (BG.Ptr F2_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF2_Aux")

instance HasCField.HasCField F2_Aux "unwrapF2_Aux" where

  type CFieldType F2_Aux "unwrapF2_Aux" =
    BG.CInt -> BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f2@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 10:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F2 = F2
  { unwrapF2 :: BG.Ptr (BG.FunPtr F2_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.FunPtr F2_Aux)
         ) => BG.CompatHasField.HasField "unwrapF2" F2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F2 {unwrapF2 = y1}, BG.getField @"unwrapF2" x0)

instance ( ty ~ BG.Ptr (BG.FunPtr F2_Aux)
         ) => BG.HasField "unwrapF2" (BG.Ptr F2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF2")

instance HasCField.HasCField F2 "unwrapF2" where

  type CFieldType F2 "unwrapF2" =
    BG.Ptr (BG.FunPtr F2_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F3'

    __C declaration:__ @f3@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 13:18@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F3_Aux = F3_Aux
  { unwrapF3_Aux :: BG.CInt -> BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_4a960721e7d1dcef_base ::
     (BG.Int32 -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ()))

-- __unique:__ @toF3_Aux@
hs_bindgen_4a960721e7d1dcef ::
     F3_Aux
  -> IO (BG.FunPtr F3_Aux)
hs_bindgen_4a960721e7d1dcef =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_4a960721e7d1dcef_base (BG.toFFIType fun0))

-- __unique:__ @fromF3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_66460422a7197535_base ::
     BG.FunPtr (BG.Int32 -> BG.Int32 -> IO ())
  -> BG.Int32 -> BG.Int32 -> IO ()

-- __unique:__ @fromF3_Aux@
hs_bindgen_66460422a7197535 ::
     BG.FunPtr F3_Aux
  -> F3_Aux
hs_bindgen_66460422a7197535 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_66460422a7197535_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F3_Aux where

  toFunPtr = hs_bindgen_4a960721e7d1dcef

instance BG.FromFunPtr F3_Aux where

  fromFunPtr = hs_bindgen_66460422a7197535

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapF3_Aux" F3_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F3_Aux {unwrapF3_Aux = y1}, BG.getField @"unwrapF3_Aux" x0)

instance ( ty ~ (BG.CInt -> BG.CInt -> IO ())
         ) => BG.HasField "unwrapF3_Aux" (BG.Ptr F3_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF3_Aux")

instance HasCField.HasCField F3_Aux "unwrapF3_Aux" where

  type CFieldType F3_Aux "unwrapF3_Aux" =
    BG.CInt -> BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f3@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 13:18@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F3 = F3
  { unwrapF3 :: BG.Ptr (BG.Ptr (BG.FunPtr F3_Aux))
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.Ptr (BG.FunPtr F3_Aux))
         ) => BG.CompatHasField.HasField "unwrapF3" F3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F3 {unwrapF3 = y1}, BG.getField @"unwrapF3" x0)

instance ( ty ~ BG.Ptr (BG.Ptr (BG.FunPtr F3_Aux))
         ) => BG.HasField "unwrapF3" (BG.Ptr F3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF3")

instance HasCField.HasCField F3 "unwrapF3" where

  type CFieldType F3 "unwrapF3" =
    BG.Ptr (BG.Ptr (BG.FunPtr F3_Aux))

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F4'

    __C declaration:__ @f4@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 16:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F4_Aux = F4_Aux
  { unwrapF4_Aux :: IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_83bcff023b3bc648_base ::
     IO BG.Int32
  -> IO (BG.FunPtr (IO BG.Int32))

-- __unique:__ @toF4_Aux@
hs_bindgen_83bcff023b3bc648 ::
     F4_Aux
  -> IO (BG.FunPtr F4_Aux)
hs_bindgen_83bcff023b3bc648 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_83bcff023b3bc648_base (BG.toFFIType fun0))

-- __unique:__ @fromF4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_40f9a8d432b9eb97_base ::
     BG.FunPtr (IO BG.Int32)
  -> IO BG.Int32

-- __unique:__ @fromF4_Aux@
hs_bindgen_40f9a8d432b9eb97 ::
     BG.FunPtr F4_Aux
  -> F4_Aux
hs_bindgen_40f9a8d432b9eb97 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_40f9a8d432b9eb97_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F4_Aux where

  toFunPtr = hs_bindgen_83bcff023b3bc648

instance BG.FromFunPtr F4_Aux where

  fromFunPtr = hs_bindgen_40f9a8d432b9eb97

instance ( ty ~ IO BG.CInt
         ) => BG.CompatHasField.HasField "unwrapF4_Aux" F4_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F4_Aux {unwrapF4_Aux = y1}, BG.getField @"unwrapF4_Aux" x0)

instance ( ty ~ IO BG.CInt
         ) => BG.HasField "unwrapF4_Aux" (BG.Ptr F4_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF4_Aux")

instance HasCField.HasCField F4_Aux "unwrapF4_Aux" where

  type CFieldType F4_Aux "unwrapF4_Aux" = IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f4@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 16:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F4 = F4
  { unwrapF4 :: BG.Ptr (BG.FunPtr F4_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.FunPtr F4_Aux)
         ) => BG.CompatHasField.HasField "unwrapF4" F4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F4 {unwrapF4 = y1}, BG.getField @"unwrapF4" x0)

instance ( ty ~ BG.Ptr (BG.FunPtr F4_Aux)
         ) => BG.HasField "unwrapF4" (BG.Ptr F4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF4")

instance HasCField.HasCField F4 "unwrapF4" where

  type CFieldType F4 "unwrapF4" =
    BG.Ptr (BG.FunPtr F4_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F5'

    __C declaration:__ @f5@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 19:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F5_Aux = F5_Aux
  { unwrapF5_Aux :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_6891cbd81d6f42b9_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toF5_Aux@
hs_bindgen_6891cbd81d6f42b9 ::
     F5_Aux
  -> IO (BG.FunPtr F5_Aux)
hs_bindgen_6891cbd81d6f42b9 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_6891cbd81d6f42b9_base (BG.toFFIType fun0))

-- __unique:__ @fromF5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_586f6635c057975f_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF5_Aux@
hs_bindgen_586f6635c057975f ::
     BG.FunPtr F5_Aux
  -> F5_Aux
hs_bindgen_586f6635c057975f =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_586f6635c057975f_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F5_Aux where

  toFunPtr = hs_bindgen_6891cbd81d6f42b9

instance BG.FromFunPtr F5_Aux where

  fromFunPtr = hs_bindgen_586f6635c057975f

instance ( ty ~ IO ()
         ) => BG.CompatHasField.HasField "unwrapF5_Aux" F5_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F5_Aux {unwrapF5_Aux = y1}, BG.getField @"unwrapF5_Aux" x0)

instance ( ty ~ IO ()
         ) => BG.HasField "unwrapF5_Aux" (BG.Ptr F5_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF5_Aux")

instance HasCField.HasCField F5_Aux "unwrapF5_Aux" where

  type CFieldType F5_Aux "unwrapF5_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f5@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 19:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F5 = F5
  { unwrapF5 :: BG.Ptr (BG.FunPtr F5_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.FunPtr F5_Aux)
         ) => BG.CompatHasField.HasField "unwrapF5" F5 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F5 {unwrapF5 = y1}, BG.getField @"unwrapF5" x0)

instance ( ty ~ BG.Ptr (BG.FunPtr F5_Aux)
         ) => BG.HasField "unwrapF5" (BG.Ptr F5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF5")

instance HasCField.HasCField F5 "unwrapF5" where

  type CFieldType F5 "unwrapF5" =
    BG.Ptr (BG.FunPtr F5_Aux)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyInt@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 22:13@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapMyInt" MyInt ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyInt {unwrapMyInt = y1}, BG.getField @"unwrapMyInt" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMyInt" (BG.Ptr MyInt) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F6'

    __C declaration:__ @f6@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 23:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F6_Aux = F6_Aux
  { unwrapF6_Aux :: MyInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toF6_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c1baf73f98614f45_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @toF6_Aux@
hs_bindgen_c1baf73f98614f45 ::
     F6_Aux
  -> IO (BG.FunPtr F6_Aux)
hs_bindgen_c1baf73f98614f45 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_c1baf73f98614f45_base (BG.toFFIType fun0))

-- __unique:__ @fromF6_Aux@
foreign import ccall safe "dynamic" hs_bindgen_a887947b26e58f0c_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @fromF6_Aux@
hs_bindgen_a887947b26e58f0c ::
     BG.FunPtr F6_Aux
  -> F6_Aux
hs_bindgen_a887947b26e58f0c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_a887947b26e58f0c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr F6_Aux where

  toFunPtr = hs_bindgen_c1baf73f98614f45

instance BG.FromFunPtr F6_Aux where

  fromFunPtr = hs_bindgen_a887947b26e58f0c

instance ( ty ~ (MyInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapF6_Aux" F6_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         F6_Aux {unwrapF6_Aux = y1}, BG.getField @"unwrapF6_Aux" x0)

instance ( ty ~ (MyInt -> IO ())
         ) => BG.HasField "unwrapF6_Aux" (BG.Ptr F6_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapF6_Aux")

instance HasCField.HasCField F6_Aux "unwrapF6_Aux" where

  type CFieldType F6_Aux "unwrapF6_Aux" =
    MyInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f6@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h 23:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/multi_level.h@
-}
newtype F6 = F6
  { unwrapF6 :: BG.Ptr (BG.FunPtr F6_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.FunPtr F6_Aux)
         ) => BG.CompatHasField.HasField "unwrapF6" F6 ty where

  hasField =
    \x0 ->
      (\y1 ->
         F6 {unwrapF6 = y1}, BG.getField @"unwrapF6" x0)

instance ( ty ~ BG.Ptr (BG.FunPtr F6_Aux)
         ) => BG.HasField "unwrapF6" (BG.Ptr F6) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF6")

instance HasCField.HasCField F6 "unwrapF6" where

  type CFieldType F6 "unwrapF6" =
    BG.Ptr (BG.FunPtr F6_Aux)

  offset# = \_ -> \_ -> 0
