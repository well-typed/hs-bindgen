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
    ( Example.I(..)
    , Example.S(..)
    , Example.U(..)
    , Example.E(..)
    , pattern Example.Foo
    , Example.TI(..)
    , Example.TS(..)
    , Example.TU(..)
    , Example.TE(..)
    , Example.TTI(..)
    , Example.TTS(..)
    , Example.TTU(..)
    , Example.TTE(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @I@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 10:13@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
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

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 11:8@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
data S = S
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S where

  readRaw = \ptr0 -> pure S

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S -> return ()

deriving via Marshal.EquivStorable S instance BG.Storable S

{-| __C declaration:__ @union U@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 12:7@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype U = U
  { unwrapU :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 0 1 instance Marshal.StaticSize U

deriving via BG.SizedByteArray 0 1 instance Marshal.ReadRaw U

deriving via BG.SizedByteArray 0 1 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance BG.Storable U

deriving via BG.SizedByteArray 0 1 instance Union.IsUnion U

{-| __C declaration:__ @enum E@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 13:6@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype E = E
  { unwrapE :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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
      CEnum.declaredValuesFromList [(0, BG.singleton "Foo")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E where

  minDeclaredValue = Foo

  maxDeclaredValue = Foo

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

{-| __C declaration:__ @foo@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 13:9@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
pattern Foo :: E
pattern Foo = E 0

{-| __C declaration:__ @TI@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 20:17@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TI = TI
  { unwrapTI :: I
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

instance (ty ~ I) => BG.CompatHasField.HasField "unwrapTI" TI ty where

  hasField =
    \x0 ->
      (\y1 ->
         TI {unwrapTI = y1}, BG.getField @"unwrapTI" x0)

instance (ty ~ I) => BG.HasField "unwrapTI" (BG.Ptr TI) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTI")

instance HasCField.HasCField TI "unwrapTI" where

  type CFieldType TI "unwrapTI" = I

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 21:24@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TS = TS
  { unwrapTS :: S
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ S) => BG.CompatHasField.HasField "unwrapTS" TS ty where

  hasField =
    \x0 ->
      (\y1 ->
         TS {unwrapTS = y1}, BG.getField @"unwrapTS" x0)

instance (ty ~ S) => BG.HasField "unwrapTS" (BG.Ptr TS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTS")

instance HasCField.HasCField TS "unwrapTS" where

  type CFieldType TS "unwrapTS" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 22:23@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TU = TU
  { unwrapTU :: U
  }
  deriving stock (BG.Generic)
  deriving newtype
    ( Union.IsUnion
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ U) => BG.CompatHasField.HasField "unwrapTU" TU ty where

  hasField =
    \x0 ->
      (\y1 ->
         TU {unwrapTU = y1}, BG.getField @"unwrapTU" x0)

instance (ty ~ U) => BG.HasField "unwrapTU" (BG.Ptr TU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTU")

instance HasCField.HasCField TU "unwrapTU" where

  type CFieldType TU "unwrapTU" = U

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 23:22@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TE = TE
  { unwrapTE :: E
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.HasFFIType
    , BG.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ E) => BG.CompatHasField.HasField "unwrapTE" TE ty where

  hasField =
    \x0 ->
      (\y1 ->
         TE {unwrapTE = y1}, BG.getField @"unwrapTE" x0)

instance (ty ~ E) => BG.HasField "unwrapTE" (BG.Ptr TE) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTE")

instance HasCField.HasCField TE "unwrapTE" where

  type CFieldType TE "unwrapTE" = E

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTI@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 30:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTI = TTI
  { unwrapTTI :: TI
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

instance (ty ~ TI) => BG.CompatHasField.HasField "unwrapTTI" TTI ty where

  hasField =
    \x0 ->
      (\y1 ->
         TTI {unwrapTTI = y1}, BG.getField @"unwrapTTI" x0)

instance (ty ~ TI) => BG.HasField "unwrapTTI" (BG.Ptr TTI) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTTI")

instance HasCField.HasCField TTI "unwrapTTI" where

  type CFieldType TTI "unwrapTTI" = TI

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 31:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTS = TTS
  { unwrapTTS :: TS
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ TS) => BG.CompatHasField.HasField "unwrapTTS" TTS ty where

  hasField =
    \x0 ->
      (\y1 ->
         TTS {unwrapTTS = y1}, BG.getField @"unwrapTTS" x0)

instance (ty ~ TS) => BG.HasField "unwrapTTS" (BG.Ptr TTS) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTTS")

instance HasCField.HasCField TTS "unwrapTTS" where

  type CFieldType TTS "unwrapTTS" = TS

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 32:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTU = TTU
  { unwrapTTU :: TU
  }
  deriving stock (BG.Generic)
  deriving newtype
    ( Union.IsUnion
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ TU) => BG.CompatHasField.HasField "unwrapTTU" TTU ty where

  hasField =
    \x0 ->
      (\y1 ->
         TTU {unwrapTTU = y1}, BG.getField @"unwrapTTU" x0)

instance (ty ~ TU) => BG.HasField "unwrapTTU" (BG.Ptr TTU) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTTU")

instance HasCField.HasCField TTU "unwrapTTU" where

  type CFieldType TTU "unwrapTTU" = TU

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 33:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTE = TTE
  { unwrapTTE :: TE
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.HasFFIType
    , BG.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ TE) => BG.CompatHasField.HasField "unwrapTTE" TTE ty where

  hasField =
    \x0 ->
      (\y1 ->
         TTE {unwrapTTE = y1}, BG.getField @"unwrapTTE" x0)

instance (ty ~ TE) => BG.HasField "unwrapTTE" (BG.Ptr TTE) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTTE")

instance HasCField.HasCField TTE "unwrapTTE" where

  type CFieldType TTE "unwrapTTE" = TE

  offset# = \_ -> \_ -> 0
