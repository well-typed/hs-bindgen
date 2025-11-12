{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Td where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Tag
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @rpm_tag_t@

    __defined at:__ @rpm\/rpmtypes.h:27:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpm_tag_t = Rpm_tag_t
  { un_Rpm_tag_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_tagtype_t@

    __defined at:__ @rpm\/rpmtypes.h:28:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpm_tagtype_t = Rpm_tagtype_t
  { un_Rpm_tagtype_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_count_t@

    __defined at:__ @rpm\/rpmtypes.h:29:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpm_count_t = Rpm_count_t
  { un_Rpm_count_t :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmTagVal@

    __defined at:__ @rpm\/rpmtypes.h:30:19@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmTagVal = RpmTagVal
  { un_RpmTagVal :: Rpm_tag_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpm_data_t@

    __defined at:__ @rpm\/rpmtypes.h:33:17@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpm_data_t = Rpm_data_t
  { un_Rpm_data_t :: Ptr.Ptr Void
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmtd@

    __defined at:__ @rpm\/rpmtypes.h:36:26@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpmtd = Rpmtd
  { un_Rpmtd :: Ptr.Ptr Rpmtd_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmFlags@

    __defined at:__ @rpm\/rpmtypes.h:42:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmFlags = RpmFlags
  { un_RpmFlags :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmsid@

    __defined at:__ @rpm\/rpmtypes.h:84:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype Rpmsid = Rpmsid
  { un_Rpmsid :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @rpmstrPool_s@

    __defined at:__ @rpm\/rpmtypes.h:85:16@

    __exported by:__ @rpm\/rpmtd.h@
-}
data RpmstrPool_s

{-| __C declaration:__ @rpmstrPool@

    __defined at:__ @rpm\/rpmtypes.h:85:31@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmstrPool = RpmstrPool
  { un_RpmstrPool :: Ptr.Ptr RpmstrPool_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > rpmtag

  The classes of data in tags from headers.

__C declaration:__ @rpmTagClass@

__defined at:__ @rpm\/rpmtag.h:500:3@

__exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmTagClass = RpmTagClass
  { un_RpmTagClass :: RPM.Tag.RpmTagClass
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable)

{-| Flags potentially present in rpmtd struct.

__C declaration:__ @rpmtdFlags_e@

__defined at:__ @rpm\/rpmtd.h:20:6@

__exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmtdFlags_e = RpmtdFlags_e
  { un_RpmtdFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmtdFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmtdFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmtdFlags_e un_RpmtdFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmtdFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum RpmtdFlags_e where

  type CEnumZ RpmtdFlags_e = FC.CUInt

  toCEnum = RpmtdFlags_e

  fromCEnum = un_RpmtdFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMTD_NONE")
                                                     , (1, Data.List.NonEmpty.singleton "RPMTD_ALLOCED")
                                                     , (2, Data.List.NonEmpty.singleton "RPMTD_PTR_ALLOCED")
                                                     , (4, Data.List.NonEmpty.singleton "RPMTD_IMMUTABLE")
                                                     , (8, Data.List.NonEmpty.singleton "RPMTD_ARGV")
                                                     , (16, Data.List.NonEmpty.singleton "RPMTD_INVALID")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmtdFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmtdFlags_e"

instance Show RpmtdFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmtdFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMTD_NONE@

    __defined at:__ @rpm\/rpmtd.h:21:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_NONE :: RpmtdFlags_e
pattern RPMTD_NONE = RpmtdFlags_e 0

{-| __C declaration:__ @RPMTD_ALLOCED@

    __defined at:__ @rpm\/rpmtd.h:22:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_ALLOCED :: RpmtdFlags_e
pattern RPMTD_ALLOCED = RpmtdFlags_e 1

{-| __C declaration:__ @RPMTD_PTR_ALLOCED@

    __defined at:__ @rpm\/rpmtd.h:23:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_PTR_ALLOCED :: RpmtdFlags_e
pattern RPMTD_PTR_ALLOCED = RpmtdFlags_e 2

{-| __C declaration:__ @RPMTD_IMMUTABLE@

    __defined at:__ @rpm\/rpmtd.h:24:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_IMMUTABLE :: RpmtdFlags_e
pattern RPMTD_IMMUTABLE = RpmtdFlags_e 4

{-| __C declaration:__ @RPMTD_ARGV@

    __defined at:__ @rpm\/rpmtd.h:25:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_ARGV :: RpmtdFlags_e
pattern RPMTD_ARGV = RpmtdFlags_e 8

{-| __C declaration:__ @RPMTD_INVALID@

    __defined at:__ @rpm\/rpmtd.h:26:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_INVALID :: RpmtdFlags_e
pattern RPMTD_INVALID = RpmtdFlags_e 16

{-| __C declaration:__ @rpmtdFlags@

    __defined at:__ @rpm\/rpmtd.h:29:18@

    __exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmtdFlags = RpmtdFlags
  { un_RpmtdFlags :: RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmtd

  Container for rpm tag data (from headers or extensions).

  __TODO:__

  Make this opaque (at least outside rpm itself)

__C declaration:__ @rpmtd_s@

__defined at:__ @rpm\/rpmtd.h:35:8@

__exported by:__ @rpm\/rpmtd.h@
-}
data Rpmtd_s = Rpmtd_s
  { rpmtd_s_tag :: Rpm_tag_t
    {- ^ rpm tag of this data entry

    __C declaration:__ @tag@

    __defined at:__ @rpm\/rpmtd.h:36:15@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_type :: Rpm_tagtype_t
    {- ^ data type

    __C declaration:__ @type@

    __defined at:__ @rpm\/rpmtd.h:37:19@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_count :: Rpm_count_t
    {- ^ number of entries

    __C declaration:__ @count@

    __defined at:__ @rpm\/rpmtd.h:38:17@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_data :: Rpm_data_t
    {- ^ pointer to actual data

    __C declaration:__ @data@

    __defined at:__ @rpm\/rpmtd.h:39:16@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_flags :: RpmtdFlags
    {- ^ flags on memory allocation etc

    __C declaration:__ @flags@

    __defined at:__ @rpm\/rpmtd.h:40:16@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_ix :: FC.CInt
    {- ^ iteration index

    __C declaration:__ @ix@

    __defined at:__ @rpm\/rpmtd.h:41:9@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  , rpmtd_s_size :: Rpm_count_t
    {- ^ size of data (only works for RPMTD_IMMUTABLE atm)

    __C declaration:__ @size@

    __defined at:__ @rpm\/rpmtd.h:42:17@

    __exported by:__ @rpm\/rpmtd.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Rpmtd_s where

  sizeOf = \_ -> (40 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Rpmtd_s
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (28 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rpmtd_s
            rpmtd_s_tag2
            rpmtd_s_type3
            rpmtd_s_count4
            rpmtd_s_data5
            rpmtd_s_flags6
            rpmtd_s_ix7
            rpmtd_s_size8 ->
                 F.pokeByteOff ptr0 (0 :: Int) rpmtd_s_tag2
              >> F.pokeByteOff ptr0 (4 :: Int) rpmtd_s_type3
              >> F.pokeByteOff ptr0 (8 :: Int) rpmtd_s_count4
              >> F.pokeByteOff ptr0 (16 :: Int) rpmtd_s_data5
              >> F.pokeByteOff ptr0 (24 :: Int) rpmtd_s_flags6
              >> F.pokeByteOff ptr0 (28 :: Int) rpmtd_s_ix7
              >> F.pokeByteOff ptr0 (32 :: Int) rpmtd_s_size8

{-| Formats supported by rpmtdFormat().

__C declaration:__ @rpmtdFormats@

__defined at:__ @rpm\/rpmtd.h:231:14@

__exported by:__ @rpm\/rpmtd.h@
-}
newtype RpmtdFormats = RpmtdFormats
  { un_RpmtdFormats :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmtdFormats where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmtdFormats
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmtdFormats un_RpmtdFormats2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmtdFormats2

instance HsBindgen.Runtime.CEnum.CEnum RpmtdFormats where

  type CEnumZ RpmtdFormats = FC.CUInt

  toCEnum = RpmtdFormats

  fromCEnum = un_RpmtdFormats

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMTD_FORMAT_STRING")
                                                     , (1, Data.List.NonEmpty.singleton "RPMTD_FORMAT_ARMOR")
                                                     , (2, Data.List.NonEmpty.singleton "RPMTD_FORMAT_BASE64")
                                                     , (3, Data.List.NonEmpty.singleton "RPMTD_FORMAT_PGPSIG")
                                                     , (4, Data.List.NonEmpty.singleton "RPMTD_FORMAT_DEPFLAGS")
                                                     , (5, Data.List.NonEmpty.singleton "RPMTD_FORMAT_FFLAGS")
                                                     , (6, Data.List.NonEmpty.singleton "RPMTD_FORMAT_PERMS")
                                                     , (7, Data.List.NonEmpty.singleton "RPMTD_FORMAT_TRIGGERTYPE")
                                                     , (8, Data.List.NonEmpty.singleton "RPMTD_FORMAT_XML")
                                                     , (9, Data.List.NonEmpty.singleton "RPMTD_FORMAT_OCTAL")
                                                     , (10, Data.List.NonEmpty.singleton "RPMTD_FORMAT_HEX")
                                                     , (11, Data.List.NonEmpty.singleton "RPMTD_FORMAT_DATE")
                                                     , (12, Data.List.NonEmpty.singleton "RPMTD_FORMAT_DAY")
                                                     , (13, Data.List.NonEmpty.singleton "RPMTD_FORMAT_SHESCAPE")
                                                     , (14, Data.List.NonEmpty.singleton "RPMTD_FORMAT_ARRAYSIZE")
                                                     , (15, Data.List.NonEmpty.singleton "RPMTD_FORMAT_DEPTYPE")
                                                     , (16, Data.List.NonEmpty.singleton "RPMTD_FORMAT_FSTATE")
                                                     , (17, Data.List.NonEmpty.singleton "RPMTD_FORMAT_VFLAGS")
                                                     , (18, Data.List.NonEmpty.singleton "RPMTD_FORMAT_EXPAND")
                                                     , (19, Data.List.NonEmpty.singleton "RPMTD_FORMAT_FSTATUS")
                                                     , (20, Data.List.NonEmpty.singleton "RPMTD_FORMAT_HUMANSI")
                                                     , (21, Data.List.NonEmpty.singleton "RPMTD_FORMAT_HUMANIEC")
                                                     , (22, Data.List.NonEmpty.singleton "RPMTD_FORMAT_TAGNAME")
                                                     , (23, Data.List.NonEmpty.singleton "RPMTD_FORMAT_TAGNUM")
                                                     , (24, Data.List.NonEmpty.singleton "RPMTD_FORMAT_JSON")
                                                     , (25, Data.List.NonEmpty.singleton "RPMTD_FORMAT_HASHALGO")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmtdFormats"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmtdFormats"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmtdFormats where

  minDeclaredValue = RPMTD_FORMAT_STRING

  maxDeclaredValue = RPMTD_FORMAT_HASHALGO

instance Show RpmtdFormats where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmtdFormats where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMTD_FORMAT_STRING@

    __defined at:__ @rpm\/rpmtd.h:232:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_STRING :: RpmtdFormats
pattern RPMTD_FORMAT_STRING = RpmtdFormats 0

{-| __C declaration:__ @RPMTD_FORMAT_ARMOR@

    __defined at:__ @rpm\/rpmtd.h:233:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_ARMOR :: RpmtdFormats
pattern RPMTD_FORMAT_ARMOR = RpmtdFormats 1

{-| __C declaration:__ @RPMTD_FORMAT_BASE64@

    __defined at:__ @rpm\/rpmtd.h:234:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_BASE64 :: RpmtdFormats
pattern RPMTD_FORMAT_BASE64 = RpmtdFormats 2

{-| __C declaration:__ @RPMTD_FORMAT_PGPSIG@

    __defined at:__ @rpm\/rpmtd.h:235:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_PGPSIG :: RpmtdFormats
pattern RPMTD_FORMAT_PGPSIG = RpmtdFormats 3

{-| __C declaration:__ @RPMTD_FORMAT_DEPFLAGS@

    __defined at:__ @rpm\/rpmtd.h:236:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_DEPFLAGS :: RpmtdFormats
pattern RPMTD_FORMAT_DEPFLAGS = RpmtdFormats 4

{-| __C declaration:__ @RPMTD_FORMAT_FFLAGS@

    __defined at:__ @rpm\/rpmtd.h:237:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_FFLAGS :: RpmtdFormats
pattern RPMTD_FORMAT_FFLAGS = RpmtdFormats 5

{-| __C declaration:__ @RPMTD_FORMAT_PERMS@

    __defined at:__ @rpm\/rpmtd.h:238:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_PERMS :: RpmtdFormats
pattern RPMTD_FORMAT_PERMS = RpmtdFormats 6

{-| __C declaration:__ @RPMTD_FORMAT_TRIGGERTYPE@

    __defined at:__ @rpm\/rpmtd.h:239:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_TRIGGERTYPE :: RpmtdFormats
pattern RPMTD_FORMAT_TRIGGERTYPE = RpmtdFormats 7

{-| __C declaration:__ @RPMTD_FORMAT_XML@

    __defined at:__ @rpm\/rpmtd.h:240:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_XML :: RpmtdFormats
pattern RPMTD_FORMAT_XML = RpmtdFormats 8

{-| __C declaration:__ @RPMTD_FORMAT_OCTAL@

    __defined at:__ @rpm\/rpmtd.h:241:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_OCTAL :: RpmtdFormats
pattern RPMTD_FORMAT_OCTAL = RpmtdFormats 9

{-| __C declaration:__ @RPMTD_FORMAT_HEX@

    __defined at:__ @rpm\/rpmtd.h:242:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_HEX :: RpmtdFormats
pattern RPMTD_FORMAT_HEX = RpmtdFormats 10

{-| __C declaration:__ @RPMTD_FORMAT_DATE@

    __defined at:__ @rpm\/rpmtd.h:243:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_DATE :: RpmtdFormats
pattern RPMTD_FORMAT_DATE = RpmtdFormats 11

{-| __C declaration:__ @RPMTD_FORMAT_DAY@

    __defined at:__ @rpm\/rpmtd.h:244:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_DAY :: RpmtdFormats
pattern RPMTD_FORMAT_DAY = RpmtdFormats 12

{-| __C declaration:__ @RPMTD_FORMAT_SHESCAPE@

    __defined at:__ @rpm\/rpmtd.h:245:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_SHESCAPE :: RpmtdFormats
pattern RPMTD_FORMAT_SHESCAPE = RpmtdFormats 13

{-| __C declaration:__ @RPMTD_FORMAT_ARRAYSIZE@

    __defined at:__ @rpm\/rpmtd.h:246:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_ARRAYSIZE :: RpmtdFormats
pattern RPMTD_FORMAT_ARRAYSIZE = RpmtdFormats 14

{-| __C declaration:__ @RPMTD_FORMAT_DEPTYPE@

    __defined at:__ @rpm\/rpmtd.h:247:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_DEPTYPE :: RpmtdFormats
pattern RPMTD_FORMAT_DEPTYPE = RpmtdFormats 15

{-| __C declaration:__ @RPMTD_FORMAT_FSTATE@

    __defined at:__ @rpm\/rpmtd.h:248:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_FSTATE :: RpmtdFormats
pattern RPMTD_FORMAT_FSTATE = RpmtdFormats 16

{-| __C declaration:__ @RPMTD_FORMAT_VFLAGS@

    __defined at:__ @rpm\/rpmtd.h:249:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_VFLAGS :: RpmtdFormats
pattern RPMTD_FORMAT_VFLAGS = RpmtdFormats 17

{-| __C declaration:__ @RPMTD_FORMAT_EXPAND@

    __defined at:__ @rpm\/rpmtd.h:250:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_EXPAND :: RpmtdFormats
pattern RPMTD_FORMAT_EXPAND = RpmtdFormats 18

{-| __C declaration:__ @RPMTD_FORMAT_FSTATUS@

    __defined at:__ @rpm\/rpmtd.h:251:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_FSTATUS :: RpmtdFormats
pattern RPMTD_FORMAT_FSTATUS = RpmtdFormats 19

{-| __C declaration:__ @RPMTD_FORMAT_HUMANSI@

    __defined at:__ @rpm\/rpmtd.h:252:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_HUMANSI :: RpmtdFormats
pattern RPMTD_FORMAT_HUMANSI = RpmtdFormats 20

{-| __C declaration:__ @RPMTD_FORMAT_HUMANIEC@

    __defined at:__ @rpm\/rpmtd.h:253:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_HUMANIEC :: RpmtdFormats
pattern RPMTD_FORMAT_HUMANIEC = RpmtdFormats 21

{-| __C declaration:__ @RPMTD_FORMAT_TAGNAME@

    __defined at:__ @rpm\/rpmtd.h:254:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_TAGNAME :: RpmtdFormats
pattern RPMTD_FORMAT_TAGNAME = RpmtdFormats 22

{-| __C declaration:__ @RPMTD_FORMAT_TAGNUM@

    __defined at:__ @rpm\/rpmtd.h:255:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_TAGNUM :: RpmtdFormats
pattern RPMTD_FORMAT_TAGNUM = RpmtdFormats 23

{-| __C declaration:__ @RPMTD_FORMAT_JSON@

    __defined at:__ @rpm\/rpmtd.h:256:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_JSON :: RpmtdFormats
pattern RPMTD_FORMAT_JSON = RpmtdFormats 24

{-| __C declaration:__ @RPMTD_FORMAT_HASHALGO@

    __defined at:__ @rpm\/rpmtd.h:257:5@

    __exported by:__ @rpm\/rpmtd.h@
-}
pattern RPMTD_FORMAT_HASHALGO :: RpmtdFormats
pattern RPMTD_FORMAT_HASHALGO = RpmtdFormats 25
