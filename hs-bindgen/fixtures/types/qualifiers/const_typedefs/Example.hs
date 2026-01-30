{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Marshal
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

{-| __C declaration:__ @I@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 10:13@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype I = I
  { unwrapI :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType I) "unwrapI")
         ) => GHC.Records.HasField "unwrapI" (Ptr.Ptr I) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapI")

instance HsBindgen.Runtime.HasCField.HasCField I "unwrapI" where

  type CFieldType I "unwrapI" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 11:8@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
data S = S
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S where

  readRaw = \ptr0 -> pure S

instance HsBindgen.Runtime.Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable S instance F.Storable S

{-| __C declaration:__ @union U@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 12:7@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype U = U
  { unwrapU :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.StaticSize U

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.ReadRaw U

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.WriteRaw U

deriving via HsBindgen.Runtime.Marshal.EquivStorable U instance F.Storable U

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance Data.Primitive.Types.Prim U

{-| __C declaration:__ @enum E@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 13:6@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype E = E
  { unwrapE :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize E where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrapE2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE2

deriving via HsBindgen.Runtime.Marshal.EquivStorable E instance F.Storable E

deriving via FC.CUInt instance Data.Primitive.Types.Prim E

instance HsBindgen.Runtime.CEnum.CEnum E where

  type CEnumZ E = FC.CUInt

  toCEnum = E

  fromCEnum = unwrapE

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "Foo")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "E"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum E where

  minDeclaredValue = Foo

  maxDeclaredValue = Foo

instance Show E where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read E where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "unwrapE")
         ) => GHC.Records.HasField "unwrapE" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapE")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = FC.CUInt

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
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TI) "unwrapTI")
         ) => GHC.Records.HasField "unwrapTI" (Ptr.Ptr TI) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTI")

instance HsBindgen.Runtime.HasCField.HasCField TI "unwrapTI" where

  type CFieldType TI "unwrapTI" = I

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 21:24@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TS = TS
  { unwrapTS :: S
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TS) "unwrapTS")
         ) => GHC.Records.HasField "unwrapTS" (Ptr.Ptr TS) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTS")

instance HsBindgen.Runtime.HasCField.HasCField TS "unwrapTS" where

  type CFieldType TS "unwrapTS" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 22:23@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TU = TU
  { unwrapTU :: U
  }
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TU) "unwrapTU")
         ) => GHC.Records.HasField "unwrapTU" (Ptr.Ptr TU) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTU")

instance HsBindgen.Runtime.HasCField.HasCField TU "unwrapTU" where

  type CFieldType TU "unwrapTU" = U

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 23:22@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TE = TE
  { unwrapTE :: E
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TE) "unwrapTE")
         ) => GHC.Records.HasField "unwrapTE" (Ptr.Ptr TE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTE")

instance HsBindgen.Runtime.HasCField.HasCField TE "unwrapTE" where

  type CFieldType TE "unwrapTE" = E

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTI@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 30:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTI = TTI
  { unwrapTTI :: TI
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTI) "unwrapTTI")
         ) => GHC.Records.HasField "unwrapTTI" (Ptr.Ptr TTI) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTTI")

instance HsBindgen.Runtime.HasCField.HasCField TTI "unwrapTTI" where

  type CFieldType TTI "unwrapTTI" = TI

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 31:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTS = TTS
  { unwrapTTS :: TS
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTS) "unwrapTTS")
         ) => GHC.Records.HasField "unwrapTTS" (Ptr.Ptr TTS) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTTS")

instance HsBindgen.Runtime.HasCField.HasCField TTS "unwrapTTS" where

  type CFieldType TTS "unwrapTTS" = TS

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 32:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTU = TTU
  { unwrapTTU :: TU
  }
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTU) "unwrapTTU")
         ) => GHC.Records.HasField "unwrapTTU" (Ptr.Ptr TTU) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTTU")

instance HsBindgen.Runtime.HasCField.HasCField TTU "unwrapTTU" where

  type CFieldType TTU "unwrapTTU" = TU

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 33:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTE = TTE
  { unwrapTTE :: TE
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTE) "unwrapTTE")
         ) => GHC.Records.HasField "unwrapTTE" (Ptr.Ptr TTE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTTE")

instance HsBindgen.Runtime.HasCField.HasCField TTE "unwrapTTE" where

  type CFieldType TTE "unwrapTTE" = TE

  offset# = \_ -> \_ -> 0
