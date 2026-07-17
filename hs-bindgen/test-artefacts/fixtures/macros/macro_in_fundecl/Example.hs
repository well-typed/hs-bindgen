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
    ( Example.I(..)
    , Example.C(..)
    , Example.F(..)
    , Example.L(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype I = I
  { unwrapI :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapI" I ty where

  hasField =
    \x0 ->
      (\y1 -> I {unwrapI = y1}, BG.getField @"unwrapI" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapI" (BG.Ptr I) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapI")

instance HasCField.HasCField I "unwrapI" where

  type CFieldType I "unwrapI" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype C = C
  { unwrapC :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapC" C ty where

  hasField =
    \x0 ->
      (\y1 -> C {unwrapC = y1}, BG.getField @"unwrapC" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapC" (BG.Ptr C) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapC")

instance HasCField.HasCField C "unwrapC" where

  type CFieldType C "unwrapC" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype F = F
  { unwrapF :: BG.CFloat
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "unwrapF" F ty where

  hasField =
    \x0 ->
      (\y1 -> F {unwrapF = y1}, BG.getField @"unwrapF" x0)

instance (ty ~ BG.CFloat) => BG.HasField "unwrapF" (BG.Ptr F) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF")

instance HasCField.HasCField F "unwrapF" where

  type CFieldType F "unwrapF" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype L = L
  { unwrapL :: BG.CLong
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

instance (ty ~ BG.CLong) => BG.CompatHasField.HasField "unwrapL" L ty where

  hasField =
    \x0 ->
      (\y1 -> L {unwrapL = y1}, BG.getField @"unwrapL" x0)

instance (ty ~ BG.CLong) => BG.HasField "unwrapL" (BG.Ptr L) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapL")

instance HasCField.HasCField L "unwrapL" where

  type CFieldType L "unwrapL" = BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype S = S
  { unwrapS :: BG.CShort
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

instance (ty ~ BG.CShort) => BG.CompatHasField.HasField "unwrapS" S ty where

  hasField =
    \x0 ->
      (\y1 -> S {unwrapS = y1}, BG.getField @"unwrapS" x0)

instance (ty ~ BG.CShort) => BG.HasField "unwrapS" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapS")

instance HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" = BG.CShort

  offset# = \_ -> \_ -> 0

-- __unique:__ @instance ToFunPtr (BG.CShort -> IO I)@
foreign import ccall safe "wrapper" hs_bindgen_03e9923d8d301cca_base ::
     (BG.Int16 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int16 -> IO BG.Int32))

-- __unique:__ @instance ToFunPtr (BG.CShort -> IO I)@
hs_bindgen_03e9923d8d301cca ::
     (BG.CShort -> IO I)
  -> IO (BG.FunPtr (BG.CShort -> IO I))
hs_bindgen_03e9923d8d301cca =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_03e9923d8d301cca_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.CShort -> IO I)@
foreign import ccall safe "dynamic" hs_bindgen_ee10a61bcb1a5d39_base ::
     BG.FunPtr (BG.Int16 -> IO BG.Int32)
  -> BG.Int16 -> IO BG.Int32

-- __unique:__ @instance FromFunPtr (BG.CShort -> IO I)@
hs_bindgen_ee10a61bcb1a5d39 ::
     BG.FunPtr (BG.CShort -> IO I)
  -> BG.CShort -> IO I
hs_bindgen_ee10a61bcb1a5d39 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_ee10a61bcb1a5d39_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.CShort -> IO I) where

  toFunPtr = hs_bindgen_03e9923d8d301cca

instance BG.FromFunPtr (BG.CShort -> IO I) where

  fromFunPtr = hs_bindgen_ee10a61bcb1a5d39

-- __unique:__ @instance ToFunPtr (S -> IO BG.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_140ec1313a27c405_base ::
     (BG.Int16 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int16 -> IO BG.Int32))

-- __unique:__ @instance ToFunPtr (S -> IO BG.CInt)@
hs_bindgen_140ec1313a27c405 ::
     (S -> IO BG.CInt)
  -> IO (BG.FunPtr (S -> IO BG.CInt))
hs_bindgen_140ec1313a27c405 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_140ec1313a27c405_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (S -> IO BG.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_e86a5fa3f995e6fb_base ::
     BG.FunPtr (BG.Int16 -> IO BG.Int32)
  -> BG.Int16 -> IO BG.Int32

-- __unique:__ @instance FromFunPtr (S -> IO BG.CInt)@
hs_bindgen_e86a5fa3f995e6fb ::
     BG.FunPtr (S -> IO BG.CInt)
  -> S -> IO BG.CInt
hs_bindgen_e86a5fa3f995e6fb =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_e86a5fa3f995e6fb_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (S -> IO BG.CInt) where

  toFunPtr = hs_bindgen_140ec1313a27c405

instance BG.FromFunPtr (S -> IO BG.CInt) where

  fromFunPtr = hs_bindgen_e86a5fa3f995e6fb
