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
    ( Example.ptrInt
    , Example.shortInt
    , Example.signedShortInt
    , Example.unsignedShortInt
    , Example.ptrPtrChar
    , Example.mTy
    , Example.Tty(..)
    , Example.uINT8_T
    , Example.bOOLEAN_T
    , Example.Boolean_T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro PtrInt@

    __defined at:__ @macros\/macro_types.h 2:9@

    __exported by:__ @macros\/macro_types.h@
-}
ptrInt :: [String]
ptrInt = ["int", "*"]

{-| __C declaration:__ @macro ShortInt@

    __defined at:__ @macros\/macro_types.h 3:9@

    __exported by:__ @macros\/macro_types.h@
-}
shortInt :: [String]
shortInt = ["short", "int"]

{-| __C declaration:__ @macro SignedShortInt@

    __defined at:__ @macros\/macro_types.h 4:9@

    __exported by:__ @macros\/macro_types.h@
-}
signedShortInt :: [String]
signedShortInt = ["signed", "short", "int"]

{-| __C declaration:__ @macro UnsignedShortInt@

    __defined at:__ @macros\/macro_types.h 5:9@

    __exported by:__ @macros\/macro_types.h@
-}
unsignedShortInt :: [String]
unsignedShortInt = ["unsigned", "short", "int"]

{-| __C declaration:__ @macro PtrPtrChar@

    __defined at:__ @macros\/macro_types.h 8:9@

    __exported by:__ @macros\/macro_types.h@
-}
ptrPtrChar :: [String]
ptrPtrChar = ["char", "*", "*"]

{-| __C declaration:__ @macro MTy@

    __defined at:__ @macros\/macro_types.h 11:9@

    __exported by:__ @macros\/macro_types.h@
-}
mTy :: [String]
mTy = ["float"]

{-| __C declaration:__ @tty@

    __defined at:__ @macros\/macro_types.h 12:13@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Tty = Tty
  { unwrapTty :: BG.CFloat
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

instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "unwrapTty" Tty ty where

  hasField =
    \x0 ->
      (\y1 ->
         Tty {unwrapTty = y1}, BG.getField @"unwrapTty" x0)

instance ( ty ~ BG.CFloat
         ) => BG.HasField "unwrapTty" (BG.Ptr Tty) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTty")

instance HasCField.HasCField Tty "unwrapTty" where

  type CFieldType Tty "unwrapTty" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro UINT8_T@

    __defined at:__ @macros\/macro_types.h 14:9@

    __exported by:__ @macros\/macro_types.h@
-}
uINT8_T :: [String]
uINT8_T = ["unsigned", "char"]

{-| __C declaration:__ @macro BOOLEAN_T@

    __defined at:__ @macros\/macro_types.h 15:9@

    __exported by:__ @macros\/macro_types.h@
-}
bOOLEAN_T :: [String]
bOOLEAN_T = ["UINT8_T"]

{-| __C declaration:__ @boolean_T@

    __defined at:__ @macros\/macro_types.h 16:19@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Boolean_T = Boolean_T
  { unwrapBoolean_T :: BG.CUChar
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

instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "unwrapBoolean_T" Boolean_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         Boolean_T {unwrapBoolean_T = y1}, BG.getField @"unwrapBoolean_T" x0)

instance ( ty ~ BG.CUChar
         ) => BG.HasField "unwrapBoolean_T" (BG.Ptr Boolean_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapBoolean_T")

instance HasCField.HasCField Boolean_T "unwrapBoolean_T" where

  type CFieldType Boolean_T "unwrapBoolean_T" =
    BG.CUChar

  offset# = \_ -> \_ -> 0
