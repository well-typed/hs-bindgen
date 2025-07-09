{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include \"macro_in_fundecl_vs_typedef.h\"\nchar testmodule_quux1 (MC arg1, TC arg2) { return quux1(arg1, arg2); }\nTC testmodule_quux2 (MC arg1, char arg2) { return quux2(arg1, arg2); }\nMC *testmodule_wam1 (float arg1, TC *arg2) { return wam1(arg1, arg2); }\nTC *testmodule_wam2 (float arg1, MC *arg2) { return wam2(arg1, arg2); }\nvoid testmodule_struct_typedef1 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }\nvoid testmodule_struct_typedef2 (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }\nvoid testmodule_struct_typedef3 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }\nvoid testmodule_struct_name1 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }\nvoid testmodule_struct_name2 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }\nvoid testmodule_struct_name3 (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }\n")

newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "testmodule_quux1" quux1 :: MC -> TC -> IO FC.CChar

foreign import ccall safe "testmodule_quux2" quux2 :: MC -> FC.CChar -> IO TC

foreign import ccall safe "testmodule_wam1" wam1 :: FC.CFloat -> (F.Ptr TC) -> IO (F.Ptr MC)

foreign import ccall safe "testmodule_wam2" wam2 :: FC.CFloat -> (F.Ptr MC) -> IO (F.Ptr TC)

data Struct1 = Struct1
  { struct1_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct1_a2

data Struct2 = Struct2
  { struct2_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_a2

data Struct3 = Struct3
  { struct3_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct3
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct3_a2

newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data Struct4 = Struct4
  { struct4_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct4
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct4_a2

foreign import ccall safe "testmodule_struct_typedef1" struct_typedef1 :: (F.Ptr Struct2) -> MC -> IO ()

foreign import ccall safe "testmodule_struct_typedef2" struct_typedef2 :: (F.Ptr Struct3_t) -> MC -> IO ()

foreign import ccall safe "testmodule_struct_typedef3" struct_typedef3 :: (F.Ptr Struct4) -> MC -> IO ()

foreign import ccall safe "testmodule_struct_name1" struct_name1 :: (F.Ptr Struct1) -> MC -> IO ()

foreign import ccall safe "testmodule_struct_name2" struct_name2 :: (F.Ptr Struct3) -> MC -> IO ()

foreign import ccall safe "testmodule_struct_name3" struct_name3 :: (F.Ptr Struct4) -> MC -> IO ()
