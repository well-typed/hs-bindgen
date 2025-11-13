{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Lib where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @headerToken_s@

    __defined at:__ @rpm\/rpmtypes.h:24:16@

    __exported by:__ @rpm\/rpmlib.h@
-}
data HeaderToken_s

{-|

  > rpmtypes

  RPM header and data retrieval types. @ {

__C declaration:__ @Header@

__defined at:__ @rpm\/rpmtypes.h:24:32@

__exported by:__ @rpm\/rpmlib.h@
-}
newtype Header = Header
  { un_Header :: Ptr.Ptr HeaderToken_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmts_s@

    __defined at:__ @rpm\/rpmtypes.h:63:16@

    __exported by:__ @rpm\/rpmlib.h@
-}
data Rpmts_s

{-|

  > rpmtypes

  The main types involved in transaction manipulation @ {

__C declaration:__ @rpmts@

__defined at:__ @rpm\/rpmtypes.h:63:26@

__exported by:__ @rpm\/rpmlib.h@
-}
newtype Rpmts = Rpmts
  { un_Rpmts :: Ptr.Ptr Rpmts_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @FD_s@

    __defined at:__ @rpm\/rpmtypes.h:100:16@

    __exported by:__ @rpm\/rpmlib.h@
-}
data FD_s

{-|

  > rpmtypes

  RPM IO file descriptor type

__C declaration:__ @FD_t@

__defined at:__ @rpm\/rpmtypes.h:100:23@

__exported by:__ @rpm\/rpmlib.h@
-}
newtype FD_t = FD_t
  { un_FD_t :: Ptr.Ptr FD_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @rpmMacroContext_s@

    __defined at:__ @rpm\/rpmlib.h:25:15@

    __exported by:__ @rpm\/rpmlib.h@
-}
data RpmMacroContext_s

{-|

  > rpmrc

  Build and install arch/os table identifiers.

  __TODO:__

  Eliminate from API.

__C declaration:__ @rpm_machtable_e@

__defined at:__ @rpm\/rpmlib.h:44:6@

__exported by:__ @rpm\/rpmlib.h@
-}
newtype Rpm_machtable_e = Rpm_machtable_e
  { un_Rpm_machtable_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Rpm_machtable_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Rpm_machtable_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rpm_machtable_e un_Rpm_machtable_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Rpm_machtable_e2

instance HsBindgen.Runtime.CEnum.CEnum Rpm_machtable_e where

  type CEnumZ Rpm_machtable_e = FC.CUInt

  toCEnum = Rpm_machtable_e

  fromCEnum = un_Rpm_machtable_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPM_MACHTABLE_INSTARCH")
                                                     , (1, Data.List.NonEmpty.singleton "RPM_MACHTABLE_INSTOS")
                                                     , (2, Data.List.NonEmpty.singleton "RPM_MACHTABLE_BUILDARCH")
                                                     , (3, Data.List.NonEmpty.singleton "RPM_MACHTABLE_BUILDOS")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Rpm_machtable_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Rpm_machtable_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Rpm_machtable_e where

  minDeclaredValue = RPM_MACHTABLE_INSTARCH

  maxDeclaredValue = RPM_MACHTABLE_BUILDOS

instance Show Rpm_machtable_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Rpm_machtable_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| Install platform architecture.

__C declaration:__ @RPM_MACHTABLE_INSTARCH@

__defined at:__ @rpm\/rpmlib.h:45:5@

__exported by:__ @rpm\/rpmlib.h@
-}
pattern RPM_MACHTABLE_INSTARCH :: Rpm_machtable_e
pattern RPM_MACHTABLE_INSTARCH = Rpm_machtable_e 0

{-| Install platform operating system.

__C declaration:__ @RPM_MACHTABLE_INSTOS@

__defined at:__ @rpm\/rpmlib.h:46:5@

__exported by:__ @rpm\/rpmlib.h@
-}
pattern RPM_MACHTABLE_INSTOS :: Rpm_machtable_e
pattern RPM_MACHTABLE_INSTOS = Rpm_machtable_e 1

{-| Build platform architecture.

__C declaration:__ @RPM_MACHTABLE_BUILDARCH@

__defined at:__ @rpm\/rpmlib.h:47:5@

__exported by:__ @rpm\/rpmlib.h@
-}
pattern RPM_MACHTABLE_BUILDARCH :: Rpm_machtable_e
pattern RPM_MACHTABLE_BUILDARCH = Rpm_machtable_e 2

{-| Build platform operating system.

__C declaration:__ @RPM_MACHTABLE_BUILDOS@

__defined at:__ @rpm\/rpmlib.h:48:5@

__exported by:__ @rpm\/rpmlib.h@
-}
pattern RPM_MACHTABLE_BUILDOS :: Rpm_machtable_e
pattern RPM_MACHTABLE_BUILDOS = Rpm_machtable_e 3

{-| __C declaration:__ @RPM_MACHTABLE_COUNT@

    __defined at:__ @rpm\/rpmlib.h:50:9@

    __exported by:__ @rpm\/rpmlib.h@
-}
rPM_MACHTABLE_COUNT :: FC.CInt
rPM_MACHTABLE_COUNT = (4 :: FC.CInt)
