{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Prob where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified RPM.Types
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @rpmProblem_s@

    __defined at:__ @rpm\/rpmprob.h:16:16@

    __exported by:__ @rpm\/rpmprob.h@
-}
data RpmProblem_s

{-| __C declaration:__ @rpmProblem@

    __defined at:__ @rpm\/rpmprob.h:16:31@

    __exported by:__ @rpm\/rpmprob.h@
-}
newtype RpmProblem = RpmProblem
  { un_RpmProblem :: Ptr.Ptr RpmProblem_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > rpmprob

  __TODO:__

  Generalize filter mechanism.

__C declaration:__ @rpmprobFilterFlags_e@

__defined at:__ @rpm\/rpmprob.h:21:6@

__exported by:__ @rpm\/rpmprob.h@
-}
newtype RpmprobFilterFlags_e = RpmprobFilterFlags_e
  { un_RpmprobFilterFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmprobFilterFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmprobFilterFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmprobFilterFlags_e un_RpmprobFilterFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmprobFilterFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum RpmprobFilterFlags_e where

  type CEnumZ RpmprobFilterFlags_e = FC.CUInt

  toCEnum = RpmprobFilterFlags_e

  fromCEnum = un_RpmprobFilterFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMPROB_FILTER_NONE")
                                                     , (1, Data.List.NonEmpty.singleton "RPMPROB_FILTER_IGNOREOS")
                                                     , (2, Data.List.NonEmpty.singleton "RPMPROB_FILTER_IGNOREARCH")
                                                     , (4, Data.List.NonEmpty.singleton "RPMPROB_FILTER_REPLACEPKG")
                                                     , (8, Data.List.NonEmpty.singleton "RPMPROB_FILTER_FORCERELOCATE")
                                                     , (16, Data.List.NonEmpty.singleton "RPMPROB_FILTER_REPLACENEWFILES")
                                                     , (32, Data.List.NonEmpty.singleton "RPMPROB_FILTER_REPLACEOLDFILES")
                                                     , (64, Data.List.NonEmpty.singleton "RPMPROB_FILTER_OLDPACKAGE")
                                                     , (128, Data.List.NonEmpty.singleton "RPMPROB_FILTER_DISKSPACE")
                                                     , (256, Data.List.NonEmpty.singleton "RPMPROB_FILTER_DISKNODES")
                                                     , (512, Data.List.NonEmpty.singleton "RPMPROB_FILTER_VERIFY")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmprobFilterFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmprobFilterFlags_e"

instance Show RpmprobFilterFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmprobFilterFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMPROB_FILTER_NONE@

    __defined at:__ @rpm\/rpmprob.h:22:5@

    __exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_NONE :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_NONE = RpmprobFilterFlags_e 0

{-| from --ignoreos

__C declaration:__ @RPMPROB_FILTER_IGNOREOS@

__defined at:__ @rpm\/rpmprob.h:23:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_IGNOREOS :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_IGNOREOS = RpmprobFilterFlags_e 1

{-| from --ignorearch

__C declaration:__ @RPMPROB_FILTER_IGNOREARCH@

__defined at:__ @rpm\/rpmprob.h:24:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_IGNOREARCH :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_IGNOREARCH = RpmprobFilterFlags_e 2

{-| from --replacepkgs

__C declaration:__ @RPMPROB_FILTER_REPLACEPKG@

__defined at:__ @rpm\/rpmprob.h:25:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_REPLACEPKG :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_REPLACEPKG = RpmprobFilterFlags_e 4

{-| from --badreloc

__C declaration:__ @RPMPROB_FILTER_FORCERELOCATE@

__defined at:__ @rpm\/rpmprob.h:26:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_FORCERELOCATE :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_FORCERELOCATE = RpmprobFilterFlags_e 8

{-| from --replacefiles

__C declaration:__ @RPMPROB_FILTER_REPLACENEWFILES@

__defined at:__ @rpm\/rpmprob.h:27:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_REPLACENEWFILES :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_REPLACENEWFILES = RpmprobFilterFlags_e 16

{-| from --replacefiles

__C declaration:__ @RPMPROB_FILTER_REPLACEOLDFILES@

__defined at:__ @rpm\/rpmprob.h:28:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_REPLACEOLDFILES :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_REPLACEOLDFILES = RpmprobFilterFlags_e 32

{-| from --oldpackage

__C declaration:__ @RPMPROB_FILTER_OLDPACKAGE@

__defined at:__ @rpm\/rpmprob.h:29:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_OLDPACKAGE :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_OLDPACKAGE = RpmprobFilterFlags_e 64

{-| from --ignoresize

__C declaration:__ @RPMPROB_FILTER_DISKSPACE@

__defined at:__ @rpm\/rpmprob.h:30:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_DISKSPACE :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_DISKSPACE = RpmprobFilterFlags_e 128

{-| from --ignoresize

__C declaration:__ @RPMPROB_FILTER_DISKNODES@

__defined at:__ @rpm\/rpmprob.h:31:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_DISKNODES :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_DISKNODES = RpmprobFilterFlags_e 256

{-| from --noverify

__C declaration:__ @RPMPROB_FILTER_VERIFY@

__defined at:__ @rpm\/rpmprob.h:32:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILTER_VERIFY :: RpmprobFilterFlags_e
pattern RPMPROB_FILTER_VERIFY = RpmprobFilterFlags_e 512

{-| __C declaration:__ @rpmprobFilterFlags@

    __defined at:__ @rpm\/rpmprob.h:35:18@

    __exported by:__ @rpm\/rpmprob.h@
-}
newtype RpmprobFilterFlags = RpmprobFilterFlags
  { un_RpmprobFilterFlags :: RPM.Types.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > rpmprob

  Enumerate transaction set problem types.

__C declaration:__ @rpmProblemType@

__defined at:__ @rpm\/rpmprob.h:40:14@

__exported by:__ @rpm\/rpmprob.h@
-}
newtype RpmProblemType = RpmProblemType
  { un_RpmProblemType :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmProblemType where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmProblemType
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmProblemType un_RpmProblemType2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmProblemType2

instance HsBindgen.Runtime.CEnum.CEnum RpmProblemType where

  type CEnumZ RpmProblemType = FC.CUInt

  toCEnum = RpmProblemType

  fromCEnum = un_RpmProblemType

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMPROB_BADARCH")
                                                     , (1, Data.List.NonEmpty.singleton "RPMPROB_BADOS")
                                                     , (2, Data.List.NonEmpty.singleton "RPMPROB_PKG_INSTALLED")
                                                     , (3, Data.List.NonEmpty.singleton "RPMPROB_BADRELOCATE")
                                                     , (4, Data.List.NonEmpty.singleton "RPMPROB_REQUIRES")
                                                     , (5, Data.List.NonEmpty.singleton "RPMPROB_CONFLICT")
                                                     , (6, Data.List.NonEmpty.singleton "RPMPROB_NEW_FILE_CONFLICT")
                                                     , (7, Data.List.NonEmpty.singleton "RPMPROB_FILE_CONFLICT")
                                                     , (8, Data.List.NonEmpty.singleton "RPMPROB_OLDPACKAGE")
                                                     , (9, Data.List.NonEmpty.singleton "RPMPROB_DISKSPACE")
                                                     , (10, Data.List.NonEmpty.singleton "RPMPROB_DISKNODES")
                                                     , (11, Data.List.NonEmpty.singleton "RPMPROB_OBSOLETES")
                                                     , (12, Data.List.NonEmpty.singleton "RPMPROB_VERIFY")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmProblemType"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmProblemType"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmProblemType where

  minDeclaredValue = RPMPROB_BADARCH

  maxDeclaredValue = RPMPROB_VERIFY

instance Show RpmProblemType where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmProblemType where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| package ... is for a different architecture

__C declaration:__ @RPMPROB_BADARCH@

__defined at:__ @rpm\/rpmprob.h:41:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_BADARCH :: RpmProblemType
pattern RPMPROB_BADARCH = RpmProblemType 0

{-| package ... is for a different operating system

__C declaration:__ @RPMPROB_BADOS@

__defined at:__ @rpm\/rpmprob.h:42:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_BADOS :: RpmProblemType
pattern RPMPROB_BADOS = RpmProblemType 1

{-| package ... is already installed

__C declaration:__ @RPMPROB_PKG_INSTALLED@

__defined at:__ @rpm\/rpmprob.h:43:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_PKG_INSTALLED :: RpmProblemType
pattern RPMPROB_PKG_INSTALLED = RpmProblemType 2

{-| path ... is not relocatable for package ...

__C declaration:__ @RPMPROB_BADRELOCATE@

__defined at:__ @rpm\/rpmprob.h:44:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_BADRELOCATE :: RpmProblemType
pattern RPMPROB_BADRELOCATE = RpmProblemType 3

{-| package ... has unsatisfied Requires: ...

__C declaration:__ @RPMPROB_REQUIRES@

__defined at:__ @rpm\/rpmprob.h:45:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_REQUIRES :: RpmProblemType
pattern RPMPROB_REQUIRES = RpmProblemType 4

{-| package ... has unsatisfied Conflicts: ...

__C declaration:__ @RPMPROB_CONFLICT@

__defined at:__ @rpm\/rpmprob.h:46:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_CONFLICT :: RpmProblemType
pattern RPMPROB_CONFLICT = RpmProblemType 5

{-| file ... conflicts between attempted installs of ...

__C declaration:__ @RPMPROB_NEW_FILE_CONFLICT@

__defined at:__ @rpm\/rpmprob.h:47:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_NEW_FILE_CONFLICT :: RpmProblemType
pattern RPMPROB_NEW_FILE_CONFLICT = RpmProblemType 6

{-| file ... from install of ... conflicts with file from package ...

__C declaration:__ @RPMPROB_FILE_CONFLICT@

__defined at:__ @rpm\/rpmprob.h:48:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_FILE_CONFLICT :: RpmProblemType
pattern RPMPROB_FILE_CONFLICT = RpmProblemType 7

{-| package ... (which is newer than ...) is already installed

__C declaration:__ @RPMPROB_OLDPACKAGE@

__defined at:__ @rpm\/rpmprob.h:49:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_OLDPACKAGE :: RpmProblemType
pattern RPMPROB_OLDPACKAGE = RpmProblemType 8

{-| installing package ... needs ... on the ... filesystem

__C declaration:__ @RPMPROB_DISKSPACE@

__defined at:__ @rpm\/rpmprob.h:50:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_DISKSPACE :: RpmProblemType
pattern RPMPROB_DISKSPACE = RpmProblemType 9

{-| installing package ... needs ... on the ... filesystem

__C declaration:__ @RPMPROB_DISKNODES@

__defined at:__ @rpm\/rpmprob.h:51:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_DISKNODES :: RpmProblemType
pattern RPMPROB_DISKNODES = RpmProblemType 10

{-| package ... is obsoleted by ...

__C declaration:__ @RPMPROB_OBSOLETES@

__defined at:__ @rpm\/rpmprob.h:52:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_OBSOLETES :: RpmProblemType
pattern RPMPROB_OBSOLETES = RpmProblemType 11

{-| package did not pass verification

__C declaration:__ @RPMPROB_VERIFY@

__defined at:__ @rpm\/rpmprob.h:53:5@

__exported by:__ @rpm\/rpmprob.h@
-}
pattern RPMPROB_VERIFY :: RpmProblemType
pattern RPMPROB_VERIFY = RpmProblemType 12
