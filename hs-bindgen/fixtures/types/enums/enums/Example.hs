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

{-| __C declaration:__ @enum first@

    __defined at:__ @types\/enums\/enums.h 4:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype First = First
  { unwrapFirst :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize First where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw First where

  readRaw =
    \ptr0 ->
          pure First
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw First where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          First unwrapFirst2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFirst2

deriving via Marshal.EquivStorable First instance RIP.Storable First

deriving via RIP.CUInt instance RIP.Prim First

instance CEnum.CEnum First where

  type CEnumZ First = RIP.CUInt

  toCEnum = First

  fromCEnum = unwrapFirst

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "FIRST1"), (1, RIP.singleton "FIRST2")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "First"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "First"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum First where

  minDeclaredValue = FIRST1

  maxDeclaredValue = FIRST2

instance Show First where

  showsPrec = CEnum.shows

instance Read First where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapFirst" (RIP.Ptr First) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFirst")

instance HasCField.HasCField First "unwrapFirst" where

  type CFieldType First "unwrapFirst" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FIRST1@

    __defined at:__ @types\/enums\/enums.h 5:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern FIRST1 :: First
pattern FIRST1 = First 0

{-| __C declaration:__ @FIRST2@

    __defined at:__ @types\/enums\/enums.h 6:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern FIRST2 :: First
pattern FIRST2 = First 1

{-| __C declaration:__ @enum second@

    __defined at:__ @types\/enums\/enums.h 9:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Second = Second
  { unwrapSecond :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Second where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Second where

  readRaw =
    \ptr0 ->
          pure Second
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Second where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Second unwrapSecond2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSecond2

deriving via Marshal.EquivStorable Second instance RIP.Storable Second

deriving via RIP.CInt instance RIP.Prim Second

instance CEnum.CEnum Second where

  type CEnumZ Second = RIP.CInt

  toCEnum = Second

  fromCEnum = unwrapSecond

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (-1, RIP.singleton "SECOND_A")
                                   , (0, RIP.singleton "SECOND_B")
                                   , (1, RIP.singleton "SECOND_C")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Second"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Second"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Second where

  minDeclaredValue = SECOND_A

  maxDeclaredValue = SECOND_C

instance Show Second where

  showsPrec = CEnum.shows

instance Read Second where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapSecond" (RIP.Ptr Second) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSecond")

instance HasCField.HasCField Second "unwrapSecond" where

  type CFieldType Second "unwrapSecond" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SECOND_A@

    __defined at:__ @types\/enums\/enums.h 10:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_A :: Second
pattern SECOND_A = Second (-1)

{-| __C declaration:__ @SECOND_B@

    __defined at:__ @types\/enums\/enums.h 11:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_B :: Second
pattern SECOND_B = Second 0

{-| __C declaration:__ @SECOND_C@

    __defined at:__ @types\/enums\/enums.h 12:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_C :: Second
pattern SECOND_C = Second 1

{-| __C declaration:__ @enum same@

    __defined at:__ @types\/enums\/enums.h 15:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Same = Same
  { unwrapSame :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Same where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Same where

  readRaw =
    \ptr0 ->
          pure Same
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Same where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same unwrapSame2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSame2

deriving via Marshal.EquivStorable Same instance RIP.Storable Same

deriving via RIP.CUInt instance RIP.Prim Same

instance CEnum.CEnum Same where

  type CEnumZ Same = RIP.CUInt

  toCEnum = Same

  fromCEnum = unwrapSame

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, ("SAME_A" RIP.:| ["SAME_B"]))]

  showsUndeclared = CEnum.showsWrappedUndeclared "Same"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Same"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Same where

  minDeclaredValue = SAME_A

  maxDeclaredValue = SAME_A

instance Show Same where

  showsPrec = CEnum.shows

instance Read Same where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapSame" (RIP.Ptr Same) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSame")

instance HasCField.HasCField Same "unwrapSame" where

  type CFieldType Same "unwrapSame" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SAME_A@

    __defined at:__ @types\/enums\/enums.h 16:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SAME_A :: Same
pattern SAME_A = Same 1

{-| __C declaration:__ @SAME_B@

    __defined at:__ @types\/enums\/enums.h 17:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SAME_B :: Same
pattern SAME_B = Same 1

{-| __C declaration:__ @enum nonseq@

    __defined at:__ @types\/enums\/enums.h 20:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Nonseq = Nonseq
  { unwrapNonseq :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Nonseq where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Nonseq where

  readRaw =
    \ptr0 ->
          pure Nonseq
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Nonseq where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Nonseq unwrapNonseq2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapNonseq2

deriving via Marshal.EquivStorable Nonseq instance RIP.Storable Nonseq

deriving via RIP.CUInt instance RIP.Prim Nonseq

instance CEnum.CEnum Nonseq where

  type CEnumZ Nonseq = RIP.CUInt

  toCEnum = Nonseq

  fromCEnum = unwrapNonseq

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (200, RIP.singleton "NONSEQ_A")
                                   , (301, RIP.singleton "NONSEQ_B")
                                   , (404, RIP.singleton "NONSEQ_C")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Nonseq"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Nonseq"

instance Show Nonseq where

  showsPrec = CEnum.shows

instance Read Nonseq where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapNonseq" (RIP.Ptr Nonseq) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapNonseq")

instance HasCField.HasCField Nonseq "unwrapNonseq" where

  type CFieldType Nonseq "unwrapNonseq" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NONSEQ_A@

    __defined at:__ @types\/enums\/enums.h 21:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_A :: Nonseq
pattern NONSEQ_A = Nonseq 200

{-| __C declaration:__ @NONSEQ_B@

    __defined at:__ @types\/enums\/enums.h 22:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_B :: Nonseq
pattern NONSEQ_B = Nonseq 301

{-| __C declaration:__ @NONSEQ_C@

    __defined at:__ @types\/enums\/enums.h 23:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_C :: Nonseq
pattern NONSEQ_C = Nonseq 404

{-| __C declaration:__ @enum packed@

    __defined at:__ @types\/enums\/enums.h 26:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Packed = Packed
  { unwrapPacked :: RIP.CUChar
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Packed where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Packed where

  readRaw =
    \ptr0 ->
          pure Packed
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Packed where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Packed unwrapPacked2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapPacked2

deriving via Marshal.EquivStorable Packed instance RIP.Storable Packed

deriving via RIP.CUChar instance RIP.Prim Packed

instance CEnum.CEnum Packed where

  type CEnumZ Packed = RIP.CUChar

  toCEnum = Packed

  fromCEnum = unwrapPacked

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "PACKED_A")
                                   , (1, RIP.singleton "PACKED_B")
                                   , (2, RIP.singleton "PACKED_C")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Packed"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Packed"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Packed where

  minDeclaredValue = PACKED_A

  maxDeclaredValue = PACKED_C

instance Show Packed where

  showsPrec = CEnum.shows

instance Read Packed where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "unwrapPacked" (RIP.Ptr Packed) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPacked")

instance HasCField.HasCField Packed "unwrapPacked" where

  type CFieldType Packed "unwrapPacked" = RIP.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @PACKED_A@

    __defined at:__ @types\/enums\/enums.h 27:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_A :: Packed
pattern PACKED_A = Packed 0

{-| __C declaration:__ @PACKED_B@

    __defined at:__ @types\/enums\/enums.h 27:15@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_B :: Packed
pattern PACKED_B = Packed 1

{-| __C declaration:__ @PACKED_C@

    __defined at:__ @types\/enums\/enums.h 27:25@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_C :: Packed
pattern PACKED_C = Packed 2

{-| __C declaration:__ @enum enumA@

    __defined at:__ @types\/enums\/enums.h 30:9@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumA = EnumA
  { unwrapEnumA :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize EnumA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw EnumA where

  readRaw =
    \ptr0 ->
          pure EnumA
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw EnumA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA unwrapEnumA2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumA2

deriving via Marshal.EquivStorable EnumA instance RIP.Storable EnumA

deriving via RIP.CUInt instance RIP.Prim EnumA

instance CEnum.CEnum EnumA where

  type CEnumZ EnumA = RIP.CUInt

  toCEnum = EnumA

  fromCEnum = unwrapEnumA

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "A_FOO"), (1, RIP.singleton "A_BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum EnumA where

  minDeclaredValue = A_FOO

  maxDeclaredValue = A_BAR

instance Show EnumA where

  showsPrec = CEnum.shows

instance Read EnumA where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapEnumA" (RIP.Ptr EnumA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEnumA")

instance HasCField.HasCField EnumA "unwrapEnumA" where

  type CFieldType EnumA "unwrapEnumA" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A_FOO@

    __defined at:__ @types\/enums\/enums.h 30:16@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern A_FOO :: EnumA
pattern A_FOO = EnumA 0

{-| __C declaration:__ @A_BAR@

    __defined at:__ @types\/enums\/enums.h 30:23@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern A_BAR :: EnumA
pattern A_BAR = EnumA 1

{-| __C declaration:__ @enum enumB@

    __defined at:__ @types\/enums\/enums.h 32:14@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumB = EnumB
  { unwrapEnumB :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize EnumB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw EnumB where

  readRaw =
    \ptr0 ->
          pure EnumB
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw EnumB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumB unwrapEnumB2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumB2

deriving via Marshal.EquivStorable EnumB instance RIP.Storable EnumB

deriving via RIP.CUInt instance RIP.Prim EnumB

instance CEnum.CEnum EnumB where

  type CEnumZ EnumB = RIP.CUInt

  toCEnum = EnumB

  fromCEnum = unwrapEnumB

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "B_FOO"), (1, RIP.singleton "B_BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "EnumB"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "EnumB"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum EnumB where

  minDeclaredValue = B_FOO

  maxDeclaredValue = B_BAR

instance Show EnumB where

  showsPrec = CEnum.shows

instance Read EnumB where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapEnumB" (RIP.Ptr EnumB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEnumB")

instance HasCField.HasCField EnumB "unwrapEnumB" where

  type CFieldType EnumB "unwrapEnumB" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B_FOO@

    __defined at:__ @types\/enums\/enums.h 32:22@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern B_FOO :: EnumB
pattern B_FOO = EnumB 0

{-| __C declaration:__ @B_BAR@

    __defined at:__ @types\/enums\/enums.h 32:29@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern B_BAR :: EnumB
pattern B_BAR = EnumB 1

{-| __C declaration:__ @enum enumC@

    __defined at:__ @types\/enums\/enums.h 34:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumC = EnumC
  { unwrapEnumC :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize EnumC where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw EnumC where

  readRaw =
    \ptr0 ->
          pure EnumC
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw EnumC where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumC unwrapEnumC2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumC2

deriving via Marshal.EquivStorable EnumC instance RIP.Storable EnumC

deriving via RIP.CUInt instance RIP.Prim EnumC

instance CEnum.CEnum EnumC where

  type CEnumZ EnumC = RIP.CUInt

  toCEnum = EnumC

  fromCEnum = unwrapEnumC

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "C_FOO"), (1, RIP.singleton "C_BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "EnumC"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "EnumC"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum EnumC where

  minDeclaredValue = C_FOO

  maxDeclaredValue = C_BAR

instance Show EnumC where

  showsPrec = CEnum.shows

instance Read EnumC where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapEnumC" (RIP.Ptr EnumC) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEnumC")

instance HasCField.HasCField EnumC "unwrapEnumC" where

  type CFieldType EnumC "unwrapEnumC" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @C_FOO@

    __defined at:__ @types\/enums\/enums.h 34:14@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern C_FOO :: EnumC
pattern C_FOO = EnumC 0

{-| __C declaration:__ @C_BAR@

    __defined at:__ @types\/enums\/enums.h 34:21@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern C_BAR :: EnumC
pattern C_BAR = EnumC 1

{-| __C declaration:__ @enum enumD@

    __defined at:__ @types\/enums\/enums.h 37:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumD_t = EnumD_t
  { unwrapEnumD_t :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize EnumD_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw EnumD_t where

  readRaw =
    \ptr0 ->
          pure EnumD_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw EnumD_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumD_t unwrapEnumD_t2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumD_t2

deriving via Marshal.EquivStorable EnumD_t instance RIP.Storable EnumD_t

deriving via RIP.CUInt instance RIP.Prim EnumD_t

instance CEnum.CEnum EnumD_t where

  type CEnumZ EnumD_t = RIP.CUInt

  toCEnum = EnumD_t

  fromCEnum = unwrapEnumD_t

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "D_FOO"), (1, RIP.singleton "D_BAR")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "EnumD_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "EnumD_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum EnumD_t where

  minDeclaredValue = D_FOO

  maxDeclaredValue = D_BAR

instance Show EnumD_t where

  showsPrec = CEnum.shows

instance Read EnumD_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapEnumD_t" (RIP.Ptr EnumD_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEnumD_t")

instance HasCField.HasCField EnumD_t "unwrapEnumD_t" where

  type CFieldType EnumD_t "unwrapEnumD_t" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @D_FOO@

    __defined at:__ @types\/enums\/enums.h 37:14@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern D_FOO :: EnumD_t
pattern D_FOO = EnumD_t 0

{-| __C declaration:__ @D_BAR@

    __defined at:__ @types\/enums\/enums.h 37:21@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern D_BAR :: EnumD_t
pattern D_BAR = EnumD_t 1
