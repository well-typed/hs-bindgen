{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, fmap, pure, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 3:13@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
newtype T = T
  { unwrapT :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 12:16@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
data S = S
  { s_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 12:24@

         __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @x@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 12:24@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_x = y1}, BG.getField @"s_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = BG.CInt

  offset# = \_ -> \_ -> 0

-- __unique:__ @instance ToFunPtr (T -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_938f852d69e4b718_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (T -> IO ())@
hs_bindgen_938f852d69e4b718 ::
     (T -> IO ())
  -> IO (BG.FunPtr (T -> IO ()))
hs_bindgen_938f852d69e4b718 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_938f852d69e4b718_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (T -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_aa40e997bef5d441_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (T -> IO ())@
hs_bindgen_aa40e997bef5d441 ::
     BG.FunPtr (T -> IO ())
  -> T -> IO ()
hs_bindgen_aa40e997bef5d441 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_aa40e997bef5d441_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (T -> IO ()) where

  toFunPtr = hs_bindgen_938f852d69e4b718

instance BG.FromFunPtr (T -> IO ()) where

  fromFunPtr = hs_bindgen_aa40e997bef5d441

-- __unique:__ @instance ToFunPtr (BG.FunPtr (T -> IO ()) -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_86e0703874ba870d_base ::
     (BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.FunPtr (T -> IO ()) -> IO ())@
hs_bindgen_86e0703874ba870d ::
     (BG.FunPtr (T -> IO ()) -> IO ())
  -> IO (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()))
hs_bindgen_86e0703874ba870d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_86e0703874ba870d_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.FunPtr (T -> IO ()) -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_d1c65e3640cf4367_base ::
     BG.FunPtr (BG.FunPtr BG.Void -> IO ())
  -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.FunPtr (T -> IO ()) -> IO ())@
hs_bindgen_d1c65e3640cf4367 ::
     BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
  -> BG.FunPtr (T -> IO ()) -> IO ()
hs_bindgen_d1c65e3640cf4367 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_d1c65e3640cf4367_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (BG.FunPtr (T -> IO ()) -> IO ()) where

  toFunPtr = hs_bindgen_86e0703874ba870d

instance BG.FromFunPtr (BG.FunPtr (T -> IO ()) -> IO ()) where

  fromFunPtr = hs_bindgen_d1c65e3640cf4367

-- __unique:__ @instance ToFunPtr (BG.Ptr S -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_3a11d92b7d8961e6_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr S -> IO ())@
hs_bindgen_3a11d92b7d8961e6 ::
     (BG.Ptr S -> IO ())
  -> IO (BG.FunPtr (BG.Ptr S -> IO ()))
hs_bindgen_3a11d92b7d8961e6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_3a11d92b7d8961e6_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.Ptr S -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_16b157993d4d5b48_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr S -> IO ())@
hs_bindgen_16b157993d4d5b48 ::
     BG.FunPtr (BG.Ptr S -> IO ())
  -> BG.Ptr S -> IO ()
hs_bindgen_16b157993d4d5b48 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_16b157993d4d5b48_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (BG.Ptr S -> IO ()) where

  toFunPtr = hs_bindgen_3a11d92b7d8961e6

instance BG.FromFunPtr (BG.Ptr S -> IO ()) where

  fromFunPtr = hs_bindgen_16b157993d4d5b48
