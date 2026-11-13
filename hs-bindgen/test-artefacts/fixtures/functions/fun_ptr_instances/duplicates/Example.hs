{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show, fmap, type (~))

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
