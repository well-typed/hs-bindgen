{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Ds where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.FunPtr
import qualified RPM.Types
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-|

  > rpmtypes

  Package read return codes.

__C declaration:__ @rpmRC@

__defined at:__ @rpm\/rpmtypes.h:111:3@

__exported by:__ @rpm\/rpmds.h@
-}
newtype RpmRC = RpmRC
  { un_RpmRC :: RPM.Types.RpmRC
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable)

{-|

  > rpmds

  Dependency Attributes.

__C declaration:__ @rpmsenseFlags_e@

__defined at:__ @rpm\/rpmds.h:22:6@

__exported by:__ @rpm\/rpmds.h@
-}
newtype RpmsenseFlags_e = RpmsenseFlags_e
  { un_RpmsenseFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmsenseFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmsenseFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmsenseFlags_e un_RpmsenseFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmsenseFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum RpmsenseFlags_e where

  type CEnumZ RpmsenseFlags_e = FC.CUInt

  toCEnum = RpmsenseFlags_e

  fromCEnum = un_RpmsenseFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMSENSE_ANY")
                                                     , (2, Data.List.NonEmpty.singleton "RPMSENSE_LESS")
                                                     , (4, Data.List.NonEmpty.singleton "RPMSENSE_GREATER")
                                                     , (8, Data.List.NonEmpty.singleton "RPMSENSE_EQUAL")
                                                     , (32, Data.List.NonEmpty.singleton "RPMSENSE_POSTTRANS")
                                                     , (64, Data.List.NonEmpty.singleton "RPMSENSE_PREREQ")
                                                     , (128, Data.List.NonEmpty.singleton "RPMSENSE_PRETRANS")
                                                     , (256, Data.List.NonEmpty.singleton "RPMSENSE_INTERP")
                                                     , (512, Data.List.NonEmpty.singleton "RPMSENSE_SCRIPT_PRE")
                                                     , (1024, Data.List.NonEmpty.singleton "RPMSENSE_SCRIPT_POST")
                                                     , (2048, Data.List.NonEmpty.singleton "RPMSENSE_SCRIPT_PREUN")
                                                     , (4096, Data.List.NonEmpty.singleton "RPMSENSE_SCRIPT_POSTUN")
                                                     , (8192, Data.List.NonEmpty.singleton "RPMSENSE_SCRIPT_VERIFY")
                                                     , (16384, Data.List.NonEmpty.singleton "RPMSENSE_FIND_REQUIRES")
                                                     , (32768, Data.List.NonEmpty.singleton "RPMSENSE_FIND_PROVIDES")
                                                     , (65536, Data.List.NonEmpty.singleton "RPMSENSE_TRIGGERIN")
                                                     , (131072, Data.List.NonEmpty.singleton "RPMSENSE_TRIGGERUN")
                                                     , (262144, Data.List.NonEmpty.singleton "RPMSENSE_TRIGGERPOSTUN")
                                                     , (524288, Data.List.NonEmpty.singleton "RPMSENSE_MISSINGOK")
                                                     , (1048576, Data.List.NonEmpty.singleton "RPMSENSE_PREUNTRANS")
                                                     , (2097152, Data.List.NonEmpty.singleton "RPMSENSE_POSTUNTRANS")
                                                     , (16777216, Data.List.NonEmpty.singleton "RPMSENSE_RPMLIB")
                                                     , (33554432, Data.List.NonEmpty.singleton "RPMSENSE_TRIGGERPREIN")
                                                     , (67108864, Data.List.NonEmpty.singleton "RPMSENSE_KEYRING")
                                                     , (268435456, Data.List.NonEmpty.singleton "RPMSENSE_CONFIG")
                                                     , (536870912, Data.List.NonEmpty.singleton "RPMSENSE_META")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmsenseFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmsenseFlags_e"

instance Show RpmsenseFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmsenseFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMSENSE_ANY@

    __defined at:__ @rpm\/rpmds.h:23:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_ANY :: RpmsenseFlags_e
pattern RPMSENSE_ANY = RpmsenseFlags_e 0

{-| __C declaration:__ @RPMSENSE_LESS@

    __defined at:__ @rpm\/rpmds.h:24:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_LESS :: RpmsenseFlags_e
pattern RPMSENSE_LESS = RpmsenseFlags_e 2

{-| __C declaration:__ @RPMSENSE_GREATER@

    __defined at:__ @rpm\/rpmds.h:25:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_GREATER :: RpmsenseFlags_e
pattern RPMSENSE_GREATER = RpmsenseFlags_e 4

{-| __C declaration:__ @RPMSENSE_EQUAL@

    __defined at:__ @rpm\/rpmds.h:26:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_EQUAL :: RpmsenseFlags_e
pattern RPMSENSE_EQUAL = RpmsenseFlags_e 8

{-| %posttrans dependency

__C declaration:__ @RPMSENSE_POSTTRANS@

__defined at:__ @rpm\/rpmds.h:28:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_POSTTRANS :: RpmsenseFlags_e
pattern RPMSENSE_POSTTRANS = RpmsenseFlags_e 32

{-| __C declaration:__ @RPMSENSE_PREREQ@

    __defined at:__ @rpm\/rpmds.h:29:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_PREREQ :: RpmsenseFlags_e
pattern RPMSENSE_PREREQ = RpmsenseFlags_e 64

{-| Pre-transaction dependency.

__C declaration:__ @RPMSENSE_PRETRANS@

__defined at:__ @rpm\/rpmds.h:30:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_PRETRANS :: RpmsenseFlags_e
pattern RPMSENSE_PRETRANS = RpmsenseFlags_e 128

{-| Interpreter used by scriptlet.

__C declaration:__ @RPMSENSE_INTERP@

__defined at:__ @rpm\/rpmds.h:31:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_INTERP :: RpmsenseFlags_e
pattern RPMSENSE_INTERP = RpmsenseFlags_e 256

{-| %pre dependency.

__C declaration:__ @RPMSENSE_SCRIPT_PRE@

__defined at:__ @rpm\/rpmds.h:32:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_SCRIPT_PRE :: RpmsenseFlags_e
pattern RPMSENSE_SCRIPT_PRE = RpmsenseFlags_e 512

{-| %post dependency.

__C declaration:__ @RPMSENSE_SCRIPT_POST@

__defined at:__ @rpm\/rpmds.h:33:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_SCRIPT_POST :: RpmsenseFlags_e
pattern RPMSENSE_SCRIPT_POST = RpmsenseFlags_e 1024

{-| %preun dependency.

__C declaration:__ @RPMSENSE_SCRIPT_PREUN@

__defined at:__ @rpm\/rpmds.h:34:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_SCRIPT_PREUN :: RpmsenseFlags_e
pattern RPMSENSE_SCRIPT_PREUN = RpmsenseFlags_e 2048

{-| %postun dependency.

__C declaration:__ @RPMSENSE_SCRIPT_POSTUN@

__defined at:__ @rpm\/rpmds.h:35:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_SCRIPT_POSTUN :: RpmsenseFlags_e
pattern RPMSENSE_SCRIPT_POSTUN = RpmsenseFlags_e 4096

{-| %verify dependency.

__C declaration:__ @RPMSENSE_SCRIPT_VERIFY@

__defined at:__ @rpm\/rpmds.h:36:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_SCRIPT_VERIFY :: RpmsenseFlags_e
pattern RPMSENSE_SCRIPT_VERIFY = RpmsenseFlags_e 8192

{-| find-requires generated dependency.

__C declaration:__ @RPMSENSE_FIND_REQUIRES@

__defined at:__ @rpm\/rpmds.h:37:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_FIND_REQUIRES :: RpmsenseFlags_e
pattern RPMSENSE_FIND_REQUIRES = RpmsenseFlags_e 16384

{-| find-provides generated dependency.

__C declaration:__ @RPMSENSE_FIND_PROVIDES@

__defined at:__ @rpm\/rpmds.h:38:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_FIND_PROVIDES :: RpmsenseFlags_e
pattern RPMSENSE_FIND_PROVIDES = RpmsenseFlags_e 32768

{-| %triggerin dependency.

__C declaration:__ @RPMSENSE_TRIGGERIN@

__defined at:__ @rpm\/rpmds.h:40:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_TRIGGERIN :: RpmsenseFlags_e
pattern RPMSENSE_TRIGGERIN = RpmsenseFlags_e 65536

{-| %triggerun dependency.

__C declaration:__ @RPMSENSE_TRIGGERUN@

__defined at:__ @rpm\/rpmds.h:41:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_TRIGGERUN :: RpmsenseFlags_e
pattern RPMSENSE_TRIGGERUN = RpmsenseFlags_e 131072

{-| %triggerpostun dependency.

__C declaration:__ @RPMSENSE_TRIGGERPOSTUN@

__defined at:__ @rpm\/rpmds.h:42:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_TRIGGERPOSTUN :: RpmsenseFlags_e
pattern RPMSENSE_TRIGGERPOSTUN = RpmsenseFlags_e 262144

{-| suggests/enhances hint.

__C declaration:__ @RPMSENSE_MISSINGOK@

__defined at:__ @rpm\/rpmds.h:43:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_MISSINGOK :: RpmsenseFlags_e
pattern RPMSENSE_MISSINGOK = RpmsenseFlags_e 524288

{-| %preuntrans dependency.

__C declaration:__ @RPMSENSE_PREUNTRANS@

__defined at:__ @rpm\/rpmds.h:44:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_PREUNTRANS :: RpmsenseFlags_e
pattern RPMSENSE_PREUNTRANS = RpmsenseFlags_e 1048576

{-| %postuntrans dependency.

__C declaration:__ @RPMSENSE_POSTUNTRANS@

__defined at:__ @rpm\/rpmds.h:45:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_POSTUNTRANS :: RpmsenseFlags_e
pattern RPMSENSE_POSTUNTRANS = RpmsenseFlags_e 2097152

{-| rpmlib(feature) dependency.

__C declaration:__ @RPMSENSE_RPMLIB@

__defined at:__ @rpm\/rpmds.h:47:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_RPMLIB :: RpmsenseFlags_e
pattern RPMSENSE_RPMLIB = RpmsenseFlags_e 16777216

{-| %triggerprein dependency.

__C declaration:__ @RPMSENSE_TRIGGERPREIN@

__defined at:__ @rpm\/rpmds.h:48:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_TRIGGERPREIN :: RpmsenseFlags_e
pattern RPMSENSE_TRIGGERPREIN = RpmsenseFlags_e 33554432

{-| __C declaration:__ @RPMSENSE_KEYRING@

    __defined at:__ @rpm\/rpmds.h:49:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_KEYRING :: RpmsenseFlags_e
pattern RPMSENSE_KEYRING = RpmsenseFlags_e 67108864

{-| __C declaration:__ @RPMSENSE_CONFIG@

    __defined at:__ @rpm\/rpmds.h:51:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_CONFIG :: RpmsenseFlags_e
pattern RPMSENSE_CONFIG = RpmsenseFlags_e 268435456

{-| meta dependency.

__C declaration:__ @RPMSENSE_META@

__defined at:__ @rpm\/rpmds.h:52:5@

__exported by:__ @rpm\/rpmds.h@
-}
pattern RPMSENSE_META :: RpmsenseFlags_e
pattern RPMSENSE_META = RpmsenseFlags_e 536870912

{-| __C declaration:__ @rpmsenseFlags@

    __defined at:__ @rpm\/rpmds.h:55:18@

    __exported by:__ @rpm\/rpmds.h@
-}
newtype RpmsenseFlags = RpmsenseFlags
  { un_RpmsenseFlags :: RPM.Types.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @RPMSENSE_SENSEMASK@

    __defined at:__ @rpm\/rpmds.h:57:9@

    __exported by:__ @rpm\/rpmds.h@
-}
rPMSENSE_SENSEMASK :: FC.CInt
rPMSENSE_SENSEMASK = (15 :: FC.CInt)

{-| __C declaration:__ @rpmrichOp@

    __defined at:__ @rpm\/rpmds.h:466:14@

    __exported by:__ @rpm\/rpmds.h@
-}
newtype RpmrichOp = RpmrichOp
  { un_RpmrichOp :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmrichOp where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmrichOp
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmrichOp un_RpmrichOp2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmrichOp2

instance HsBindgen.Runtime.CEnum.CEnum RpmrichOp where

  type CEnumZ RpmrichOp = FC.CUInt

  toCEnum = RpmrichOp

  fromCEnum = un_RpmrichOp

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "RPMRICHOP_NONE")
                                                     , (1, Data.List.NonEmpty.singleton "RPMRICHOP_SINGLE")
                                                     , (2, Data.List.NonEmpty.singleton "RPMRICHOP_AND")
                                                     , (3, Data.List.NonEmpty.singleton "RPMRICHOP_OR")
                                                     , (4, Data.List.NonEmpty.singleton "RPMRICHOP_IF")
                                                     , (5, Data.List.NonEmpty.singleton "RPMRICHOP_ELSE")
                                                     , (6, Data.List.NonEmpty.singleton "RPMRICHOP_WITH")
                                                     , (7, Data.List.NonEmpty.singleton "RPMRICHOP_WITHOUT")
                                                     , (8, Data.List.NonEmpty.singleton "RPMRICHOP_UNLESS")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmrichOp"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmrichOp"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmrichOp where

  minDeclaredValue = RPMRICHOP_NONE

  maxDeclaredValue = RPMRICHOP_UNLESS

instance Show RpmrichOp where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmrichOp where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMRICHOP_NONE@

    __defined at:__ @rpm\/rpmds.h:467:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_NONE :: RpmrichOp
pattern RPMRICHOP_NONE = RpmrichOp 0

{-| __C declaration:__ @RPMRICHOP_SINGLE@

    __defined at:__ @rpm\/rpmds.h:468:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_SINGLE :: RpmrichOp
pattern RPMRICHOP_SINGLE = RpmrichOp 1

{-| __C declaration:__ @RPMRICHOP_AND@

    __defined at:__ @rpm\/rpmds.h:469:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_AND :: RpmrichOp
pattern RPMRICHOP_AND = RpmrichOp 2

{-| __C declaration:__ @RPMRICHOP_OR@

    __defined at:__ @rpm\/rpmds.h:470:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_OR :: RpmrichOp
pattern RPMRICHOP_OR = RpmrichOp 3

{-| __C declaration:__ @RPMRICHOP_IF@

    __defined at:__ @rpm\/rpmds.h:471:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_IF :: RpmrichOp
pattern RPMRICHOP_IF = RpmrichOp 4

{-| __C declaration:__ @RPMRICHOP_ELSE@

    __defined at:__ @rpm\/rpmds.h:472:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_ELSE :: RpmrichOp
pattern RPMRICHOP_ELSE = RpmrichOp 5

{-| __C declaration:__ @RPMRICHOP_WITH@

    __defined at:__ @rpm\/rpmds.h:473:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_WITH :: RpmrichOp
pattern RPMRICHOP_WITH = RpmrichOp 6

{-| __C declaration:__ @RPMRICHOP_WITHOUT@

    __defined at:__ @rpm\/rpmds.h:474:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_WITHOUT :: RpmrichOp
pattern RPMRICHOP_WITHOUT = RpmrichOp 7

{-| __C declaration:__ @RPMRICHOP_UNLESS@

    __defined at:__ @rpm\/rpmds.h:475:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICHOP_UNLESS :: RpmrichOp
pattern RPMRICHOP_UNLESS = RpmrichOp 8

{-| __C declaration:__ @rpmrichParseType@

    __defined at:__ @rpm\/rpmds.h:478:14@

    __exported by:__ @rpm\/rpmds.h@
-}
newtype RpmrichParseType = RpmrichParseType
  { un_RpmrichParseType :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable RpmrichParseType where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure RpmrichParseType
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          RpmrichParseType un_RpmrichParseType2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_RpmrichParseType2

instance HsBindgen.Runtime.CEnum.CEnum RpmrichParseType where

  type CEnumZ RpmrichParseType = FC.CUInt

  toCEnum = RpmrichParseType

  fromCEnum = un_RpmrichParseType

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (1, Data.List.NonEmpty.singleton "RPMRICH_PARSE_SIMPLE")
                                                     , (2, Data.List.NonEmpty.singleton "RPMRICH_PARSE_ENTER")
                                                     , (3, Data.List.NonEmpty.singleton "RPMRICH_PARSE_LEAVE")
                                                     , (4, Data.List.NonEmpty.singleton "RPMRICH_PARSE_OP")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "RpmrichParseType"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "RpmrichParseType"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum RpmrichParseType where

  minDeclaredValue = RPMRICH_PARSE_SIMPLE

  maxDeclaredValue = RPMRICH_PARSE_OP

instance Show RpmrichParseType where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read RpmrichParseType where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @RPMRICH_PARSE_SIMPLE@

    __defined at:__ @rpm\/rpmds.h:479:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICH_PARSE_SIMPLE :: RpmrichParseType
pattern RPMRICH_PARSE_SIMPLE = RpmrichParseType 1

{-| __C declaration:__ @RPMRICH_PARSE_ENTER@

    __defined at:__ @rpm\/rpmds.h:480:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICH_PARSE_ENTER :: RpmrichParseType
pattern RPMRICH_PARSE_ENTER = RpmrichParseType 2

{-| __C declaration:__ @RPMRICH_PARSE_LEAVE@

    __defined at:__ @rpm\/rpmds.h:481:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICH_PARSE_LEAVE :: RpmrichParseType
pattern RPMRICH_PARSE_LEAVE = RpmrichParseType 3

{-| __C declaration:__ @RPMRICH_PARSE_OP@

    __defined at:__ @rpm\/rpmds.h:482:5@

    __exported by:__ @rpm\/rpmds.h@
-}
pattern RPMRICH_PARSE_OP :: RpmrichParseType
pattern RPMRICH_PARSE_OP = RpmrichParseType 4

{-| Auxiliary type used by 'RpmrichParseFunction'

__defined at:__ @rpm\/rpmds.h:485:17@

__exported by:__ @rpm\/rpmds.h@
-}
newtype RpmrichParseFunction_Deref = RpmrichParseFunction_Deref
  { un_RpmrichParseFunction_Deref :: (Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC
  }

foreign import ccall safe "wrapper" toRpmrichParseFunction_Deref ::
     RpmrichParseFunction_Deref
  -> IO (Ptr.FunPtr RpmrichParseFunction_Deref)

foreign import ccall safe "dynamic" fromRpmrichParseFunction_Deref ::
     Ptr.FunPtr RpmrichParseFunction_Deref
  -> RpmrichParseFunction_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr RpmrichParseFunction_Deref where

  toFunPtr = toRpmrichParseFunction_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr RpmrichParseFunction_Deref where

  fromFunPtr = fromRpmrichParseFunction_Deref

{-| __C declaration:__ @rpmrichParseFunction@

    __defined at:__ @rpm\/rpmds.h:485:17@

    __exported by:__ @rpm\/rpmds.h@
-}
newtype RpmrichParseFunction = RpmrichParseFunction
  { un_RpmrichParseFunction :: Ptr.FunPtr RpmrichParseFunction_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

foreign import ccall safe "wrapper" funPtr_b77fd836_to ::
     ((Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC))

foreign import ccall safe "dynamic" funPtr_b77fd836_from ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC)
  -> (Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC) where

  toFunPtr = funPtr_b77fd836_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Void) -> RpmrichParseType -> (Ptr.Ptr FC.CChar) -> FC.CInt -> (Ptr.Ptr FC.CChar) -> FC.CInt -> RpmsenseFlags -> RpmrichOp -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC) where

  fromFunPtr = funPtr_b77fd836_from
