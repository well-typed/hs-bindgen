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
    , Example.S_Aux(..)
    , Example.S
    )
  where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, fmap, pure, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @functions\/fun_ptr_instances\/flam.h 3:13@

    __exported by:__ @functions\/fun_ptr_instances\/flam.h@
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

    __defined at:__ @functions\/fun_ptr_instances\/flam.h 6:8@

    __exported by:__ @functions\/fun_ptr_instances\/flam.h@
-}
data S_Aux = S
  { s_n :: BG.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @functions\/fun_ptr_instances\/flam.h 7:7@

         __exported by:__ @functions\/fun_ptr_instances\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S_Aux where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_n") ptr0

instance Marshal.WriteRaw S_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_n2 ->
            HasCField.writeRaw (BG.Proxy @"s_n") ptr0 s_n2

deriving via Marshal.EquivStorable S_Aux instance BG.Storable S_Aux

deriving via Struct.IsStructViaReadRaw S_Aux instance Struct.IsStruct S_Aux

{-| __C declaration:__ @n@

    __defined at:__ @functions\/fun_ptr_instances\/flam.h 7:7@

    __exported by:__ @functions\/fun_ptr_instances\/flam.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s_n" S_Aux ty where

  hasField =
    \x0 -> (\y1 -> S {s_n = y1}, BG.getField @"s_n" x0)

instance (ty ~ BG.CInt) => BG.HasField "s_n" (BG.Ptr S_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_n")

instance HasCField.HasCField S_Aux "s_n" where

  type CFieldType S_Aux "s_n" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset (BG.FunPtr (T -> IO ())) S_Aux where

  offset = \_proxy0 -> 8

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/fun_ptr_instances\/flam.h 6:8@

    __exported by:__ @functions\/fun_ptr_instances\/flam.h@
-}
type S = FLAM.WithFlam (BG.FunPtr (T -> IO ())) S_Aux

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
