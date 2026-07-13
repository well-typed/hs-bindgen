{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Uint8_enum(..)
    , pattern Example.U8_ZERO
    , pattern Example.U8_127
    , pattern Example.U8_128
    , pattern Example.U8_255
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @enum uint8_enum@

    __defined at:__ @types\/enums\/enum_unsigned_values.h 4:9@

    __exported by:__ @types\/enums\/enum_unsigned_values.h@
-}
newtype Uint8_enum = Uint8_enum
  { unwrapUint8_enum :: HsBindgen.Runtime.LibC.Word8
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Uint8_enum where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Uint8_enum where

  readRaw =
    \ptr0 ->
          pure Uint8_enum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Uint8_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Uint8_enum unwrapUint8_enum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapUint8_enum2

deriving via Marshal.EquivStorable Uint8_enum instance BG.Storable Uint8_enum

deriving via HsBindgen.Runtime.LibC.Word8 instance BG.Prim Uint8_enum

instance CEnum.CEnum Uint8_enum where

  type CEnumZ Uint8_enum = HsBindgen.Runtime.LibC.Word8

  toCEnum = Uint8_enum

  fromCEnum = BG.getField @"unwrapUint8_enum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, BG.singleton "U8_ZERO")
                                   , (127, BG.singleton "U8_127")
                                   , (128, BG.singleton "U8_128")
                                   , (255, BG.singleton "U8_255")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Uint8_enum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Uint8_enum"

instance Show Uint8_enum where

  showsPrec = CEnum.shows

instance Read Uint8_enum where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.CompatHasField.HasField "unwrapUint8_enum" Uint8_enum ty where

  hasField =
    \x0 ->
      ( \y1 -> Uint8_enum {unwrapUint8_enum = y1}
      , BG.getField @"unwrapUint8_enum" x0
      )

instance ( ty ~ HsBindgen.Runtime.LibC.Word8
         ) => BG.HasField "unwrapUint8_enum" (BG.Ptr Uint8_enum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint8_enum")

instance HasCField.HasCField Uint8_enum "unwrapUint8_enum" where

  type CFieldType Uint8_enum "unwrapUint8_enum" =
    HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U8_ZERO@

    __defined at:__ @types\/enums\/enum_unsigned_values.h 5:3@

    __exported by:__ @types\/enums\/enum_unsigned_values.h@
-}
pattern U8_ZERO :: Uint8_enum
pattern U8_ZERO = Uint8_enum 0

{-| __C declaration:__ @U8_127@

    __defined at:__ @types\/enums\/enum_unsigned_values.h 6:3@

    __exported by:__ @types\/enums\/enum_unsigned_values.h@
-}
pattern U8_127 :: Uint8_enum
pattern U8_127 = Uint8_enum 127

{-| __C declaration:__ @U8_128@

    __defined at:__ @types\/enums\/enum_unsigned_values.h 7:3@

    __exported by:__ @types\/enums\/enum_unsigned_values.h@
-}
pattern U8_128 :: Uint8_enum
pattern U8_128 = Uint8_enum 128

{-| __C declaration:__ @U8_255@

    __defined at:__ @types\/enums\/enum_unsigned_values.h 8:3@

    __exported by:__ @types\/enums\/enum_unsigned_values.h@
-}
pattern U8_255 :: Uint8_enum
pattern U8_255 = Uint8_enum 255
