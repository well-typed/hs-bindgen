{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.E(..)
    , pattern Example.A
    , pattern Example.B
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @enum E@

    __defined at:__ @edge-cases\/enum_bitfield.h 2:6@

    __exported by:__ @edge-cases\/enum_bitfield.h@
-}
newtype E = E
  { unwrapE :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.Bitfield, BG.HasFFIType)

instance Marshal.StaticSize E where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrapE2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE2

deriving via Marshal.EquivStorable E instance BG.Storable E

deriving via BG.CUInt instance BG.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = BG.CUInt

  toCEnum = E

  fromCEnum = BG.getField @"unwrapE"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "A"), (1, BG.singleton "B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E where

  minDeclaredValue = A

  maxDeclaredValue = B

instance Show E where

  showsPrec = CEnum.shows

instance Read E where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @edge-cases\/enum_bitfield.h 2:10@

    __exported by:__ @edge-cases\/enum_bitfield.h@
-}
pattern A :: E
pattern A = E 0

{-| __C declaration:__ @B@

    __defined at:__ @edge-cases\/enum_bitfield.h 2:13@

    __exported by:__ @edge-cases\/enum_bitfield.h@
-}
pattern B :: E
pattern B = E 1

{-| __C declaration:__ @struct S@

    __defined at:__ @edge-cases\/enum_bitfield.h 4:8@

    __exported by:__ @edge-cases\/enum_bitfield.h@
-}
data S = S
  { s_e :: E
    {- ^ __C declaration:__ @e@

         __defined at:__ @edge-cases\/enum_bitfield.h 5:10@

         __exported by:__ @edge-cases\/enum_bitfield.h@
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
      <*> HasCBitfield.peek (BG.Proxy @"s_e") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_e2 ->
            HasCBitfield.poke (BG.Proxy @"s_e") ptr0 s_e2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @e@

    __defined at:__ @edge-cases\/enum_bitfield.h 5:10@

    __exported by:__ @edge-cases\/enum_bitfield.h@
-}
instance (ty ~ E) => BG.CompatHasField.HasField "s_e" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_e = y1}, BG.getField @"s_e" x0)

instance ( ty ~ E
         ) => BG.HasField "s_e" (BG.Ptr S) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"s_e")

instance HasCBitfield.HasCBitfield S "s_e" where

  type CBitfieldType S "s_e" = E

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2
