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

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show, fmap, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @functions\/fun_ptr_instances\/array_param.h 3:13@

    __exported by:__ @functions\/fun_ptr_instances\/array_param.h@
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

-- __unique:__ @instance ToFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_51580f6056b315c6_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())@
hs_bindgen_51580f6056b315c6 ::
     (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())
  -> IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()))
hs_bindgen_51580f6056b315c6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_51580f6056b315c6_base (\x1 ->
                                                            fun0 (BG.fromFFIType x1)))

-- __unique:__ @instance FromFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_461f765741b54ba8_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())@
hs_bindgen_461f765741b54ba8 ::
     BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())
  -> BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()
hs_bindgen_461f765741b54ba8 =
  \funPtr0 ->
    \x1 ->
      hs_bindgen_461f765741b54ba8_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)

instance BG.ToFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()) where

  toFunPtr = hs_bindgen_51580f6056b315c6

instance BG.FromFunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()) where

  fromFunPtr = hs_bindgen_461f765741b54ba8
