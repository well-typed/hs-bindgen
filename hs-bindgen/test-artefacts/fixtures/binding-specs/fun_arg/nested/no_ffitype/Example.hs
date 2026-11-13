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
import qualified M
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show, fmap, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h@
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

-- __unique:__ @instance ToFunPtr (M.C -> T -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_3eb9be414bf7de25_base ::
     (BG.CInt -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))

-- __unique:__ @instance ToFunPtr (M.C -> T -> IO ())@
hs_bindgen_3eb9be414bf7de25 ::
     (M.C -> T -> IO ())
  -> IO (BG.FunPtr (M.C -> T -> IO ()))
hs_bindgen_3eb9be414bf7de25 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_3eb9be414bf7de25_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (M.C -> T -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_d3bf4e8d987645da_base ::
     BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
  -> BG.CInt -> BG.CInt -> IO ()

-- __unique:__ @instance FromFunPtr (M.C -> T -> IO ())@
hs_bindgen_d3bf4e8d987645da ::
     BG.FunPtr (M.C -> T -> IO ())
  -> M.C -> T -> IO ()
hs_bindgen_d3bf4e8d987645da =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_d3bf4e8d987645da_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)

instance BG.ToFunPtr (M.C -> T -> IO ()) where

  toFunPtr = hs_bindgen_3eb9be414bf7de25

instance BG.FromFunPtr (M.C -> T -> IO ()) where

  fromFunPtr = hs_bindgen_d3bf4e8d987645da
