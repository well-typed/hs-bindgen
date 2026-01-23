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
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

{-| __C declaration:__ @I@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 10:13@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype I = I
  { un_I :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType I) "un_I")
         ) => GHC.Records.HasField "un_I" (Ptr.Ptr I) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_I")

instance HsBindgen.Runtime.HasCField.HasCField I "un_I" where

  type CFieldType I "un_I" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 11:8@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
data S = S
  {}
  deriving stock (Eq, Show)

instance F.Storable S where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure S

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S -> return ()

instance Data.Primitive.Types.Prim S where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> S

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, S #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S -> s3

  indexOffAddr# = \addr0 -> \i1 -> S

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, S #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S -> s3

{-| __C declaration:__ @union U@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 12:7@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype U = U
  { un_U :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance F.Storable U

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance Data.Primitive.Types.Prim U

{-| __C declaration:__ @enum E@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 13:6@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype E = E
  { un_E :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance F.Storable E where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure E
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          E un_E2 -> F.pokeByteOff ptr0 (0 :: Int) un_E2

deriving via FC.CUInt instance Data.Primitive.Types.Prim E

instance HsBindgen.Runtime.CEnum.CEnum E where

  type CEnumZ E = FC.CUInt

  toCEnum = E

  fromCEnum = un_E

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

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read E where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

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
  { un_TI :: I
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TI) "un_TI")
         ) => GHC.Records.HasField "un_TI" (Ptr.Ptr TI) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TI")

instance HsBindgen.Runtime.HasCField.HasCField TI "un_TI" where

  type CFieldType TI "un_TI" = I

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 21:24@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TS = TS
  { un_TS :: S
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TS) "un_TS")
         ) => GHC.Records.HasField "un_TS" (Ptr.Ptr TS) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TS")

instance HsBindgen.Runtime.HasCField.HasCField TS "un_TS" where

  type CFieldType TS "un_TS" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 22:23@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TU = TU
  { un_TU :: U
  }
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TU) "un_TU")
         ) => GHC.Records.HasField "un_TU" (Ptr.Ptr TU) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TU")

instance HsBindgen.Runtime.HasCField.HasCField TU "un_TU" where

  type CFieldType TU "un_TU" = U

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 23:22@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TE = TE
  { un_TE :: E
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TE) "un_TE")
         ) => GHC.Records.HasField "un_TE" (Ptr.Ptr TE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TE")

instance HsBindgen.Runtime.HasCField.HasCField TE "un_TE" where

  type CFieldType TE "un_TE" = E

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTI@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 30:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTI = TTI
  { un_TTI :: TI
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTI) "un_TTI")
         ) => GHC.Records.HasField "un_TTI" (Ptr.Ptr TTI) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TTI")

instance HsBindgen.Runtime.HasCField.HasCField TTI "un_TTI" where

  type CFieldType TTI "un_TTI" = TI

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTS@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 31:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTS = TTS
  { un_TTS :: TS
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTS) "un_TTS")
         ) => GHC.Records.HasField "un_TTS" (Ptr.Ptr TTS) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TTS")

instance HsBindgen.Runtime.HasCField.HasCField TTS "un_TTS" where

  type CFieldType TTS "un_TTS" = TS

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTU@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 32:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTU = TTU
  { un_TTU :: TU
  }
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTU) "un_TTU")
         ) => GHC.Records.HasField "un_TTU" (Ptr.Ptr TTU) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TTU")

instance HsBindgen.Runtime.HasCField.HasCField TTU "un_TTU" where

  type CFieldType TTU "un_TTU" = TU

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TTE@

    __defined at:__ @types\/qualifiers\/const_typedefs.h 33:12@

    __exported by:__ @types\/qualifiers\/const_typedefs.h@
-}
newtype TTE = TTE
  { un_TTE :: TE
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TTE) "un_TTE")
         ) => GHC.Records.HasField "un_TTE" (Ptr.Ptr TTE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_TTE")

instance HsBindgen.Runtime.HasCField.HasCField TTE "un_TTE" where

  type CFieldType TTE "un_TTE" = TE

  offset# = \_ -> \_ -> 0
