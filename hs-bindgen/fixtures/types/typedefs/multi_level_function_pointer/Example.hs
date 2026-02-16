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

{-| Auxiliary type used by 'F1'

__C declaration:__ @f1@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 7:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F1_Aux = F1_Aux
  { unwrapF1_Aux :: RIP.CInt -> RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     (RIP.Int32 -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (RIP.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_00d16e666202ed6c_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ())
  -> RIP.Int32 -> RIP.Int32 -> IO ()

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

instance ( ((~) ty) (RIP.CInt -> RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapF1_Aux" (RIP.Ptr F1_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF1_Aux")

instance HasCField.HasCField F1_Aux "unwrapF1_Aux" where

  type CFieldType F1_Aux "unwrapF1_Aux" =
    RIP.CInt -> RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 7:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
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

{-| Auxiliary type used by 'F2'

__C declaration:__ @f2@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 10:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2_Aux = F2_Aux
  { unwrapF2_Aux :: RIP.CInt -> RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c39d7524b75b54e8_base ::
     (RIP.Int32 -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ()))

-- __unique:__ @toF2_Aux@
hs_bindgen_c39d7524b75b54e8 ::
     F2_Aux
  -> IO (RIP.FunPtr F2_Aux)
hs_bindgen_c39d7524b75b54e8 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c39d7524b75b54e8_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_e15bcd26f1ed1df7_base ::
     RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ())
  -> RIP.Int32 -> RIP.Int32 -> IO ()

-- __unique:__ @fromF2_Aux@
hs_bindgen_e15bcd26f1ed1df7 ::
     RIP.FunPtr F2_Aux
  -> F2_Aux
hs_bindgen_e15bcd26f1ed1df7 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_e15bcd26f1ed1df7_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F2_Aux where

  toFunPtr = hs_bindgen_c39d7524b75b54e8

instance RIP.FromFunPtr F2_Aux where

  fromFunPtr = hs_bindgen_e15bcd26f1ed1df7

instance ( ((~) ty) (RIP.CInt -> RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapF2_Aux" (RIP.Ptr F2_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF2_Aux")

instance HasCField.HasCField F2_Aux "unwrapF2_Aux" where

  type CFieldType F2_Aux "unwrapF2_Aux" =
    RIP.CInt -> RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f2@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 10:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2 = F2
  { unwrapF2 :: RIP.Ptr (RIP.FunPtr F2_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.FunPtr F2_Aux))
         ) => RIP.HasField "unwrapF2" (RIP.Ptr F2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF2")

instance HasCField.HasCField F2 "unwrapF2" where

  type CFieldType F2 "unwrapF2" =
    RIP.Ptr (RIP.FunPtr F2_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F3'

__C declaration:__ @f3@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 13:18@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3_Aux = F3_Aux
  { unwrapF3_Aux :: RIP.CInt -> RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_4a960721e7d1dcef_base ::
     (RIP.Int32 -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ()))

-- __unique:__ @toF3_Aux@
hs_bindgen_4a960721e7d1dcef ::
     F3_Aux
  -> IO (RIP.FunPtr F3_Aux)
hs_bindgen_4a960721e7d1dcef =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_4a960721e7d1dcef_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_66460422a7197535_base ::
     RIP.FunPtr (RIP.Int32 -> RIP.Int32 -> IO ())
  -> RIP.Int32 -> RIP.Int32 -> IO ()

-- __unique:__ @fromF3_Aux@
hs_bindgen_66460422a7197535 ::
     RIP.FunPtr F3_Aux
  -> F3_Aux
hs_bindgen_66460422a7197535 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_66460422a7197535_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F3_Aux where

  toFunPtr = hs_bindgen_4a960721e7d1dcef

instance RIP.FromFunPtr F3_Aux where

  fromFunPtr = hs_bindgen_66460422a7197535

instance ( ((~) ty) (RIP.CInt -> RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapF3_Aux" (RIP.Ptr F3_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF3_Aux")

instance HasCField.HasCField F3_Aux "unwrapF3_Aux" where

  type CFieldType F3_Aux "unwrapF3_Aux" =
    RIP.CInt -> RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f3@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 13:18@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3 = F3
  { unwrapF3 :: RIP.Ptr (RIP.Ptr (RIP.FunPtr F3_Aux))
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr (RIP.FunPtr F3_Aux)))
         ) => RIP.HasField "unwrapF3" (RIP.Ptr F3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF3")

instance HasCField.HasCField F3 "unwrapF3" where

  type CFieldType F3 "unwrapF3" =
    RIP.Ptr (RIP.Ptr (RIP.FunPtr F3_Aux))

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F4'

__C declaration:__ @f4@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 16:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4_Aux = F4_Aux
  { unwrapF4_Aux :: IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_83bcff023b3bc648_base ::
     IO RIP.Int32
  -> IO (RIP.FunPtr (IO RIP.Int32))

-- __unique:__ @toF4_Aux@
hs_bindgen_83bcff023b3bc648 ::
     F4_Aux
  -> IO (RIP.FunPtr F4_Aux)
hs_bindgen_83bcff023b3bc648 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_83bcff023b3bc648_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_40f9a8d432b9eb97_base ::
     RIP.FunPtr (IO RIP.Int32)
  -> IO RIP.Int32

-- __unique:__ @fromF4_Aux@
hs_bindgen_40f9a8d432b9eb97 ::
     RIP.FunPtr F4_Aux
  -> F4_Aux
hs_bindgen_40f9a8d432b9eb97 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_40f9a8d432b9eb97_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F4_Aux where

  toFunPtr = hs_bindgen_83bcff023b3bc648

instance RIP.FromFunPtr F4_Aux where

  fromFunPtr = hs_bindgen_40f9a8d432b9eb97

instance ( ((~) ty) (IO RIP.CInt)
         ) => RIP.HasField "unwrapF4_Aux" (RIP.Ptr F4_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF4_Aux")

instance HasCField.HasCField F4_Aux "unwrapF4_Aux" where

  type CFieldType F4_Aux "unwrapF4_Aux" = IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f4@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 16:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4 = F4
  { unwrapF4 :: RIP.Ptr (RIP.FunPtr F4_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.FunPtr F4_Aux))
         ) => RIP.HasField "unwrapF4" (RIP.Ptr F4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF4")

instance HasCField.HasCField F4 "unwrapF4" where

  type CFieldType F4 "unwrapF4" =
    RIP.Ptr (RIP.FunPtr F4_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F5'

__C declaration:__ @f5@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 19:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5_Aux = F5_Aux
  { unwrapF5_Aux :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_6891cbd81d6f42b9_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toF5_Aux@
hs_bindgen_6891cbd81d6f42b9 ::
     F5_Aux
  -> IO (RIP.FunPtr F5_Aux)
hs_bindgen_6891cbd81d6f42b9 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_6891cbd81d6f42b9_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_586f6635c057975f_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF5_Aux@
hs_bindgen_586f6635c057975f ::
     RIP.FunPtr F5_Aux
  -> F5_Aux
hs_bindgen_586f6635c057975f =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_586f6635c057975f_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F5_Aux where

  toFunPtr = hs_bindgen_6891cbd81d6f42b9

instance RIP.FromFunPtr F5_Aux where

  fromFunPtr = hs_bindgen_586f6635c057975f

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapF5_Aux" (RIP.Ptr F5_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF5_Aux")

instance HasCField.HasCField F5_Aux "unwrapF5_Aux" where

  type CFieldType F5_Aux "unwrapF5_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f5@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 19:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5 = F5
  { unwrapF5 :: RIP.Ptr (RIP.FunPtr F5_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.FunPtr F5_Aux))
         ) => RIP.HasField "unwrapF5" (RIP.Ptr F5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF5")

instance HasCField.HasCField F5 "unwrapF5" where

  type CFieldType F5 "unwrapF5" =
    RIP.Ptr (RIP.FunPtr F5_Aux)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyInt@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 22:13@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: RIP.CInt
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
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F6'

__C declaration:__ @f6@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 23:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6_Aux = F6_Aux
  { unwrapF6_Aux :: MyInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c1baf73f98614f45_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toF6_Aux@
hs_bindgen_c1baf73f98614f45 ::
     F6_Aux
  -> IO (RIP.FunPtr F6_Aux)
hs_bindgen_c1baf73f98614f45 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c1baf73f98614f45_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_a887947b26e58f0c_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromF6_Aux@
hs_bindgen_a887947b26e58f0c ::
     RIP.FunPtr F6_Aux
  -> F6_Aux
hs_bindgen_a887947b26e58f0c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_a887947b26e58f0c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr F6_Aux where

  toFunPtr = hs_bindgen_c1baf73f98614f45

instance RIP.FromFunPtr F6_Aux where

  fromFunPtr = hs_bindgen_a887947b26e58f0c

instance ( ((~) ty) (MyInt -> IO ())
         ) => RIP.HasField "unwrapF6_Aux" (RIP.Ptr F6_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapF6_Aux")

instance HasCField.HasCField F6_Aux "unwrapF6_Aux" where

  type CFieldType F6_Aux "unwrapF6_Aux" =
    MyInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f6@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 23:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6 = F6
  { unwrapF6 :: RIP.Ptr (RIP.FunPtr F6_Aux)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.FunPtr F6_Aux))
         ) => RIP.HasField "unwrapF6" (RIP.Ptr F6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF6")

instance HasCField.HasCField F6 "unwrapF6" where

  type CFieldType F6 "unwrapF6" =
    RIP.Ptr (RIP.FunPtr F6_Aux)

  offset# = \_ -> \_ -> 0
