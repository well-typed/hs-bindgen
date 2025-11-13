{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Header where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified RPM.Td
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-|

  > header

  Include calculation for 8 bytes of (magic, 0)?

__C declaration:__ @hMagic@

__defined at:__ @rpm\/header.h:32:6@

__exported by:__ @rpm\/header.h@
-}
newtype HMagic = HMagic
  { un_HMagic :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable HMagic where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HMagic
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HMagic un_HMagic2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HMagic2

instance HsBindgen.Runtime.CEnum.CEnum HMagic where

  type CEnumZ HMagic = FC.CUInt

  toCEnum = HMagic

  fromCEnum = un_HMagic

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "HEADER_MAGIC_NO")
                                                     , (1, Data.List.NonEmpty.singleton "HEADER_MAGIC_YES")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HMagic"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HMagic"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum HMagic where

  minDeclaredValue = HEADER_MAGIC_NO

  maxDeclaredValue = HEADER_MAGIC_YES

instance Show HMagic where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HMagic where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @HEADER_MAGIC_NO@

    __defined at:__ @rpm\/header.h:33:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADER_MAGIC_NO :: HMagic
pattern HEADER_MAGIC_NO = HMagic 0

{-| __C declaration:__ @HEADER_MAGIC_YES@

    __defined at:__ @rpm\/header.h:34:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADER_MAGIC_YES :: HMagic
pattern HEADER_MAGIC_YES = HMagic 1

{-| __C declaration:__ @headerImportFlags_e@

    __defined at:__ @rpm\/header.h:89:6@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderImportFlags_e = HeaderImportFlags_e
  { un_HeaderImportFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable HeaderImportFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HeaderImportFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HeaderImportFlags_e un_HeaderImportFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HeaderImportFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum HeaderImportFlags_e where

  type CEnumZ HeaderImportFlags_e = FC.CUInt

  toCEnum = HeaderImportFlags_e

  fromCEnum = un_HeaderImportFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (1, Data.List.NonEmpty.singleton "HEADERIMPORT_COPY")
                                                     , (2, Data.List.NonEmpty.singleton "HEADERIMPORT_FAST")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HeaderImportFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HeaderImportFlags_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum HeaderImportFlags_e where

  minDeclaredValue = HEADERIMPORT_COPY

  maxDeclaredValue = HEADERIMPORT_FAST

instance Show HeaderImportFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HeaderImportFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @HEADERIMPORT_COPY@

    __defined at:__ @rpm\/header.h:90:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERIMPORT_COPY :: HeaderImportFlags_e
pattern HEADERIMPORT_COPY = HeaderImportFlags_e 1

{-| __C declaration:__ @HEADERIMPORT_FAST@

    __defined at:__ @rpm\/header.h:91:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERIMPORT_FAST :: HeaderImportFlags_e
pattern HEADERIMPORT_FAST = HeaderImportFlags_e 2

{-| __C declaration:__ @headerImportFlags@

    __defined at:__ @rpm\/header.h:94:18@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderImportFlags = HeaderImportFlags
  { un_HeaderImportFlags :: RPM.Td.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > header

  Modifier flags for headerGet() operation. For consistent behavior you'll probably want to use ALLOC to ensure the caller owns the data, but MINMEM is useful for avoiding extra copy of data when you are sure the header wont go away. Most of the time you'll probably want EXT too, but note that extensions tags don't generally honor the other flags, MINMEM, RAW, ALLOC and ARGV are only relevant for non-extension data.

__C declaration:__ @headerGetFlags_e@

__defined at:__ @rpm\/header.h:139:6@

__exported by:__ @rpm\/header.h@
-}
newtype HeaderGetFlags_e = HeaderGetFlags_e
  { un_HeaderGetFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable HeaderGetFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HeaderGetFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HeaderGetFlags_e un_HeaderGetFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HeaderGetFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum HeaderGetFlags_e where

  type CEnumZ HeaderGetFlags_e = FC.CUInt

  toCEnum = HeaderGetFlags_e

  fromCEnum = un_HeaderGetFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "HEADERGET_DEFAULT")
                                                     , (1, Data.List.NonEmpty.singleton "HEADERGET_MINMEM")
                                                     , (2, Data.List.NonEmpty.singleton "HEADERGET_EXT")
                                                     , (4, Data.List.NonEmpty.singleton "HEADERGET_RAW")
                                                     , (8, Data.List.NonEmpty.singleton "HEADERGET_ALLOC")
                                                     , (16, Data.List.NonEmpty.singleton "HEADERGET_ARGV")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HeaderGetFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HeaderGetFlags_e"

instance Show HeaderGetFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HeaderGetFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @HEADERGET_DEFAULT@

    __defined at:__ @rpm\/header.h:140:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_DEFAULT :: HeaderGetFlags_e
pattern HEADERGET_DEFAULT = HeaderGetFlags_e 0

{-| __C declaration:__ @HEADERGET_MINMEM@

    __defined at:__ @rpm\/header.h:141:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_MINMEM :: HeaderGetFlags_e
pattern HEADERGET_MINMEM = HeaderGetFlags_e 1

{-| __C declaration:__ @HEADERGET_EXT@

    __defined at:__ @rpm\/header.h:142:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_EXT :: HeaderGetFlags_e
pattern HEADERGET_EXT = HeaderGetFlags_e 2

{-| __C declaration:__ @HEADERGET_RAW@

    __defined at:__ @rpm\/header.h:143:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_RAW :: HeaderGetFlags_e
pattern HEADERGET_RAW = HeaderGetFlags_e 4

{-| __C declaration:__ @HEADERGET_ALLOC@

    __defined at:__ @rpm\/header.h:144:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_ALLOC :: HeaderGetFlags_e
pattern HEADERGET_ALLOC = HeaderGetFlags_e 8

{-| __C declaration:__ @HEADERGET_ARGV@

    __defined at:__ @rpm\/header.h:145:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERGET_ARGV :: HeaderGetFlags_e
pattern HEADERGET_ARGV = HeaderGetFlags_e 16

{-| __C declaration:__ @headerGetFlags@

    __defined at:__ @rpm\/header.h:148:18@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderGetFlags = HeaderGetFlags
  { un_HeaderGetFlags :: RPM.Td.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @headerPutFlags_e@

    __defined at:__ @rpm\/header.h:161:6@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderPutFlags_e = HeaderPutFlags_e
  { un_HeaderPutFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable HeaderPutFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HeaderPutFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HeaderPutFlags_e un_HeaderPutFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HeaderPutFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum HeaderPutFlags_e where

  type CEnumZ HeaderPutFlags_e = FC.CUInt

  toCEnum = HeaderPutFlags_e

  fromCEnum = un_HeaderPutFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "HEADERPUT_DEFAULT")
                                                     , (1, Data.List.NonEmpty.singleton "HEADERPUT_APPEND")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HeaderPutFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HeaderPutFlags_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum HeaderPutFlags_e where

  minDeclaredValue = HEADERPUT_DEFAULT

  maxDeclaredValue = HEADERPUT_APPEND

instance Show HeaderPutFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HeaderPutFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @HEADERPUT_DEFAULT@

    __defined at:__ @rpm\/header.h:162:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERPUT_DEFAULT :: HeaderPutFlags_e
pattern HEADERPUT_DEFAULT = HeaderPutFlags_e 0

{-| __C declaration:__ @HEADERPUT_APPEND@

    __defined at:__ @rpm\/header.h:163:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERPUT_APPEND :: HeaderPutFlags_e
pattern HEADERPUT_APPEND = HeaderPutFlags_e 1

{-| __C declaration:__ @headerPutFlags@

    __defined at:__ @rpm\/header.h:166:18@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderPutFlags = HeaderPutFlags
  { un_HeaderPutFlags :: RPM.Td.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @headerConvOps@

    __defined at:__ @rpm\/header.h:341:14@

    __exported by:__ @rpm\/header.h@
-}
newtype HeaderConvOps = HeaderConvOps
  { un_HeaderConvOps :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable HeaderConvOps where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HeaderConvOps
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HeaderConvOps un_HeaderConvOps2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HeaderConvOps2

instance HsBindgen.Runtime.CEnum.CEnum HeaderConvOps where

  type CEnumZ HeaderConvOps = FC.CUInt

  toCEnum = HeaderConvOps

  fromCEnum = un_HeaderConvOps

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "HEADERCONV_EXPANDFILELIST")
                                                     , (1, Data.List.NonEmpty.singleton "HEADERCONV_COMPRESSFILELIST")
                                                     , (2, Data.List.NonEmpty.singleton "HEADERCONV_RETROFIT_V3")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HeaderConvOps"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HeaderConvOps"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum HeaderConvOps where

  minDeclaredValue = HEADERCONV_EXPANDFILELIST

  maxDeclaredValue = HEADERCONV_RETROFIT_V3

instance Show HeaderConvOps where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HeaderConvOps where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @HEADERCONV_EXPANDFILELIST@

    __defined at:__ @rpm\/header.h:342:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERCONV_EXPANDFILELIST :: HeaderConvOps
pattern HEADERCONV_EXPANDFILELIST = HeaderConvOps 0

{-| __C declaration:__ @HEADERCONV_COMPRESSFILELIST@

    __defined at:__ @rpm\/header.h:343:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERCONV_COMPRESSFILELIST :: HeaderConvOps
pattern HEADERCONV_COMPRESSFILELIST = HeaderConvOps 1

{-| __C declaration:__ @HEADERCONV_RETROFIT_V3@

    __defined at:__ @rpm\/header.h:344:5@

    __exported by:__ @rpm\/header.h@
-}
pattern HEADERCONV_RETROFIT_V3 :: HeaderConvOps
pattern HEADERCONV_RETROFIT_V3 = HeaderConvOps 2
