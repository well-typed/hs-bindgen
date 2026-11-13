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
    , Example.F(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show, fmap, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 3:13@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
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

{-| __C declaration:__ @F@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 7:20@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
newtype F = F
  { unwrapF :: IO ()
  }
  deriving stock (BG.Generic)

-- __unique:__ @toF@
foreign import ccall safe "wrapper" hs_bindgen_ec3947c3928618e6_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toF@
hs_bindgen_ec3947c3928618e6 ::
     F
  -> IO (BG.FunPtr F)
hs_bindgen_ec3947c3928618e6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_ec3947c3928618e6_base (BG.getField @"unwrapF" fun0))

-- __unique:__ @fromF@
foreign import ccall safe "dynamic" hs_bindgen_4de86d83e18d0bbf_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF@
hs_bindgen_4de86d83e18d0bbf ::
     BG.FunPtr F
  -> F
hs_bindgen_4de86d83e18d0bbf =
  \funPtr0 ->
    F (hs_bindgen_4de86d83e18d0bbf_base (BG.castFunPtr funPtr0))

instance BG.ToFunPtr F where

  toFunPtr = hs_bindgen_ec3947c3928618e6

instance BG.FromFunPtr F where

  fromFunPtr = hs_bindgen_4de86d83e18d0bbf

instance (ty ~ IO ()) => BG.CompatHasField.HasField "unwrapF" F ty where

  hasField =
    \x0 ->
      (\y1 -> F {unwrapF = y1}, BG.getField @"unwrapF" x0)

instance (ty ~ IO ()) => BG.HasField "unwrapF" (BG.Ptr F) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF")

instance HasCField.HasCField F "unwrapF" where

  type CFieldType F "unwrapF" = IO ()

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
