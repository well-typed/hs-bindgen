{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @I@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 10:13@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
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

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 11:8@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
data S = S
  {}
  deriving stock (Eq, RIP.Generic, Show)

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

deriving via Marshal.EquivStorable S instance RIP.Storable S

{-| __C declaration:__ @union U@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 12:7@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype U = U
  { unwrapU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.StaticSize U

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.ReadRaw U

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance RIP.Storable U

{-| __C declaration:__ @enum E@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 13:6@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype E = E
  { unwrapE :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

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

deriving via Marshal.EquivStorable E instance RIP.Storable E

deriving via RIP.CUInt instance RIP.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = RIP.CUInt

  toCEnum = E

  fromCEnum = unwrapE

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "Foo")]

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

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapE" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = RIP.CUInt

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

instance (((~) ty) I) => RIP.HasField "unwrapTI" (RIP.Ptr TI) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTI")

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
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) S) => RIP.HasField "unwrapTS" (RIP.Ptr TS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTS")

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
  deriving stock (RIP.Generic)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) U) => RIP.HasField "unwrapTU" (RIP.Ptr TU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTU")

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
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) E) => RIP.HasField "unwrapTE" (RIP.Ptr TE) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTE")

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

instance ( ((~) ty) TI
         ) => RIP.HasField "unwrapTTI" (RIP.Ptr TTI) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTTI")

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
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) TS
         ) => RIP.HasField "unwrapTTS" (RIP.Ptr TTS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTTS")

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
  deriving stock (RIP.Generic)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) TU
         ) => RIP.HasField "unwrapTTU" (RIP.Ptr TTU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTTU")

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
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) TE
         ) => RIP.HasField "unwrapTTE" (RIP.Ptr TTE) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTTE")

instance HasCField.HasCField TTE "unwrapTTE" where

  type CFieldType TTE "unwrapTTE" = TE

  offset# = \_ -> \_ -> 0
