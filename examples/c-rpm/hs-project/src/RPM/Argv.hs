{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module RPM.Argv where

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
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @ARGV_t@

    __defined at:__ @rpm\/argv.h:17:17@

    __exported by:__ @rpm\/argv.h@
-}
newtype ARGV_t = ARGV_t
  { un_ARGV_t :: Ptr.Ptr (Ptr.Ptr FC.CChar)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ARGV_const_t@

    __defined at:__ @rpm\/argv.h:18:23@

    __exported by:__ @rpm\/argv.h@
-}
newtype ARGV_const_t = ARGV_const_t
  { un_ARGV_const_t :: Ptr.Ptr (Ptr.Ptr FC.CChar)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ARGint_t@

    __defined at:__ @rpm\/argv.h:20:15@

    __exported by:__ @rpm\/argv.h@
-}
newtype ARGint_t = ARGint_t
  { un_ARGint_t :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ARGI_s@

    __defined at:__ @rpm\/argv.h:21:8@

    __exported by:__ @rpm\/argv.h@
-}
data ARGI_s = ARGI_s
  { aRGI_s_nvals :: FC.CUInt
    {- ^ __C declaration:__ @nvals@

         __defined at:__ @rpm\/argv.h:22:14@

         __exported by:__ @rpm\/argv.h@
    -}
  , aRGI_s_vals :: ARGint_t
    {- ^ __C declaration:__ @vals@

         __defined at:__ @rpm\/argv.h:23:14@

         __exported by:__ @rpm\/argv.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable ARGI_s where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure ARGI_s
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ARGI_s aRGI_s_nvals2 aRGI_s_vals3 ->
               F.pokeByteOff ptr0 (0 :: Int) aRGI_s_nvals2
            >> F.pokeByteOff ptr0 (8 :: Int) aRGI_s_vals3

{-| __C declaration:__ @ARGI_t@

    __defined at:__ @rpm\/argv.h:25:25@

    __exported by:__ @rpm\/argv.h@
-}
newtype ARGI_t = ARGI_t
  { un_ARGI_t :: Ptr.Ptr ARGI_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ARGI_const_t@

    __defined at:__ @rpm\/argv.h:26:37@

    __exported by:__ @rpm\/argv.h@
-}
newtype ARGI_const_t = ARGI_const_t
  { un_ARGI_const_t :: Ptr.Ptr ARGI_s
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @argvFlags_e@

    __defined at:__ @rpm\/argv.h:153:6@

    __exported by:__ @rpm\/argv.h@
-}
newtype ArgvFlags_e = ArgvFlags_e
  { un_ArgvFlags_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable ArgvFlags_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ArgvFlags_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ArgvFlags_e un_ArgvFlags_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_ArgvFlags_e2

instance HsBindgen.Runtime.CEnum.CEnum ArgvFlags_e where

  type CEnumZ ArgvFlags_e = FC.CUInt

  toCEnum = ArgvFlags_e

  fromCEnum = un_ArgvFlags_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "ARGV_NONE")
                                                     , (1, Data.List.NonEmpty.singleton "ARGV_SKIPEMPTY")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "ArgvFlags_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "ArgvFlags_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum ArgvFlags_e where

  minDeclaredValue = ARGV_NONE

  maxDeclaredValue = ARGV_SKIPEMPTY

instance Show ArgvFlags_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read ArgvFlags_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @ARGV_NONE@

    __defined at:__ @rpm\/argv.h:154:5@

    __exported by:__ @rpm\/argv.h@
-}
pattern ARGV_NONE :: ArgvFlags_e
pattern ARGV_NONE = ArgvFlags_e 0

{-| __C declaration:__ @ARGV_SKIPEMPTY@

    __defined at:__ @rpm\/argv.h:155:5@

    __exported by:__ @rpm\/argv.h@
-}
pattern ARGV_SKIPEMPTY :: ArgvFlags_e
pattern ARGV_SKIPEMPTY = ArgvFlags_e 1

{-| __C declaration:__ @argvFlags@

    __defined at:__ @rpm\/argv.h:158:18@

    __exported by:__ @rpm\/argv.h@
-}
newtype ArgvFlags = ArgvFlags
  { un_ArgvFlags :: RPM.Types.RpmFlags
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
