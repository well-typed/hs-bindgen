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

{-| __C declaration:__ @I@

    __defined at:__ @macros\/macro_in_fundecl.h 5:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype I = I
  { unwrapI :: RIP.CInt
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
         ) => RIP.HasField "unwrapI" (RIP.Ptr I) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapI")

instance HasCField.HasCField I "unwrapI" where

  type CFieldType I "unwrapI" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @C@

    __defined at:__ @macros\/macro_in_fundecl.h 6:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype C = C
  { unwrapC :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapC" (RIP.Ptr C) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapC")

instance HasCField.HasCField C "unwrapC" where

  type CFieldType C "unwrapC" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @F@

    __defined at:__ @macros\/macro_in_fundecl.h 7:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype F = F
  { unwrapF :: RIP.CFloat
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , RIP.HasFFIType
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "unwrapF" (RIP.Ptr F) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapF")

instance HasCField.HasCField F "unwrapF" where

  type CFieldType F "unwrapF" = RIP.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @L@

    __defined at:__ @macros\/macro_in_fundecl.h 8:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype L = L
  { unwrapL :: RIP.CLong
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

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "unwrapL" (RIP.Ptr L) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapL")

instance HasCField.HasCField L "unwrapL" where

  type CFieldType L "unwrapL" = RIP.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @S@

    __defined at:__ @macros\/macro_in_fundecl.h 9:9@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
newtype S = S
  { unwrapS :: RIP.CShort
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

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "unwrapS" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapS")

instance HasCField.HasCField S "unwrapS" where

  type CFieldType S "unwrapS" = RIP.CShort

  offset# = \_ -> \_ -> 0

foreign import ccall safe "wrapper" hs_bindgen_25a28556cfbbf031_base ::
     (RIP.Int16 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int16 -> IO RIP.Int32))

-- __unique:__ @instance ToFunPtr (RIP.CShort -> IO I)@
hs_bindgen_25a28556cfbbf031 ::
     (RIP.CShort -> IO I)
  -> IO (RIP.FunPtr (RIP.CShort -> IO I))
hs_bindgen_25a28556cfbbf031 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_25a28556cfbbf031_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_255a05599df0464e_base ::
     RIP.FunPtr (RIP.Int16 -> IO RIP.Int32)
  -> RIP.Int16 -> IO RIP.Int32

-- __unique:__ @instance FromFunPtr (RIP.CShort -> IO I)@
hs_bindgen_255a05599df0464e ::
     RIP.FunPtr (RIP.CShort -> IO I)
  -> RIP.CShort -> IO I
hs_bindgen_255a05599df0464e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_255a05599df0464e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr (RIP.CShort -> IO I) where

  toFunPtr = hs_bindgen_25a28556cfbbf031

instance RIP.FromFunPtr (RIP.CShort -> IO I) where

  fromFunPtr = hs_bindgen_255a05599df0464e

foreign import ccall safe "wrapper" hs_bindgen_016f1407db09bd2f_base ::
     (RIP.Int16 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int16 -> IO RIP.Int32))

-- __unique:__ @instance ToFunPtr (S -> IO RIP.CInt)@
hs_bindgen_016f1407db09bd2f ::
     (S -> IO RIP.CInt)
  -> IO (RIP.FunPtr (S -> IO RIP.CInt))
hs_bindgen_016f1407db09bd2f =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_016f1407db09bd2f_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_1c14ce64db2e04d0_base ::
     RIP.FunPtr (RIP.Int16 -> IO RIP.Int32)
  -> RIP.Int16 -> IO RIP.Int32

-- __unique:__ @instance FromFunPtr (S -> IO RIP.CInt)@
hs_bindgen_1c14ce64db2e04d0 ::
     RIP.FunPtr (S -> IO RIP.CInt)
  -> S -> IO RIP.CInt
hs_bindgen_1c14ce64db2e04d0 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_1c14ce64db2e04d0_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr (S -> IO RIP.CInt) where

  toFunPtr = hs_bindgen_016f1407db09bd2f

instance RIP.FromFunPtr (S -> IO RIP.CInt) where

  fromFunPtr = hs_bindgen_1c14ce64db2e04d0
