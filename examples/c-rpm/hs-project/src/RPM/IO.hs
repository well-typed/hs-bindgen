{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.IO where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified RPM.Sw
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @__off_t@

    __defined at:__ @bits\/types.h:152:25@

    __exported by:__ @rpm\/rpmio.h@
-}
newtype C__Off_t = C__Off_t
  { un_C__Off_t :: FC.CLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @ssize_t@

    __defined at:__ @sys\/types.h:108:19@

    __exported by:__ @rpm\/rpmio.h@
-}
newtype Ssize_t = Ssize_t
  { un_Ssize_t :: RPM.Sw.C__Ssize_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @off_t@

    __defined at:__ @sys\/types.h:85:17@

    __exported by:__ @rpm\/rpmio.h@
-}
newtype Off_t = Off_t
  { un_Off_t :: C__Off_t
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmio

  Identify per-desciptor I/O operation statistics.

__C declaration:__ @fdOpX@

__defined at:__ @rpm\/rpmio.h:132:14@

__exported by:__ @rpm\/rpmio.h@
-}
newtype FdOpX = FdOpX
  { un_FdOpX :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable FdOpX where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure FdOpX
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FdOpX un_FdOpX2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_FdOpX2

instance HsBindgen.Runtime.CEnum.CEnum FdOpX where

  type CEnumZ FdOpX = FC.CUInt

  toCEnum = FdOpX

  fromCEnum = un_FdOpX

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "FDSTAT_READ")
                                                     , (1, Data.List.NonEmpty.singleton "FDSTAT_WRITE")
                                                     , (2, Data.List.NonEmpty.singleton "FDSTAT_SEEK")
                                                     , (3, Data.List.NonEmpty.singleton "FDSTAT_CLOSE")
                                                     , (4, Data.List.NonEmpty.singleton "FDSTAT_DIGEST")
                                                     , (5, Data.List.NonEmpty.singleton "FDSTAT_MAX")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "FdOpX"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "FdOpX"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum FdOpX where

  minDeclaredValue = FDSTAT_READ

  maxDeclaredValue = FDSTAT_MAX

instance Show FdOpX where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read FdOpX where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| Read statistics index.

__C declaration:__ @FDSTAT_READ@

__defined at:__ @rpm\/rpmio.h:133:5@

__exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_READ :: FdOpX
pattern FDSTAT_READ = FdOpX 0

{-| Write statistics index.

__C declaration:__ @FDSTAT_WRITE@

__defined at:__ @rpm\/rpmio.h:134:5@

__exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_WRITE :: FdOpX
pattern FDSTAT_WRITE = FdOpX 1

{-| Seek statistics index.

__C declaration:__ @FDSTAT_SEEK@

__defined at:__ @rpm\/rpmio.h:135:5@

__exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_SEEK :: FdOpX
pattern FDSTAT_SEEK = FdOpX 2

{-| Close statistics index

__C declaration:__ @FDSTAT_CLOSE@

__defined at:__ @rpm\/rpmio.h:136:5@

__exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_CLOSE :: FdOpX
pattern FDSTAT_CLOSE = FdOpX 3

{-| Digest statistics index.

__C declaration:__ @FDSTAT_DIGEST@

__defined at:__ @rpm\/rpmio.h:137:5@

__exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_DIGEST :: FdOpX
pattern FDSTAT_DIGEST = FdOpX 4

{-| __C declaration:__ @FDSTAT_MAX@

    __defined at:__ @rpm\/rpmio.h:138:5@

    __exported by:__ @rpm\/rpmio.h@
-}
pattern FDSTAT_MAX :: FdOpX
pattern FDSTAT_MAX = FdOpX 5
