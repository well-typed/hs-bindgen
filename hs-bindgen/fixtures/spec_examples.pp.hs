{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
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
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(CAPI.addCSource "#include <spec_examples.h>\nvoid hs_bindgen_test_spec_examples_bab0544b0c2274da (int32_T *arg1, cint16_T *arg2, int64_T arg3, int64_T arg4, cint16_T *arg5) { resample(arg1, arg2, arg3, arg4, arg5); }\n")

{-| Examples from the initial specification

  __from C:__ @int16_T@
-}
newtype Int16_T = Int16_T
  { un_Int16_T :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Int32_T = Int32_T
  { un_Int32_T :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Int64_T = Int64_T
  { un_Int64_T :: FC.CLLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Cint16_T = Cint16_T
  { cint16_T_re :: Int16_T
  , cint16_T_im :: Int16_T
  }
  deriving stock (Eq, Show)

instance F.Storable Cint16_T where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Cint16_T
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cint16_T cint16_T_re2 cint16_T_im3 ->
               F.pokeByteOff ptr0 (0 :: Int) cint16_T_re2
            >> F.pokeByteOff ptr0 (2 :: Int) cint16_T_im3

data B = B
  {}
  deriving stock (Eq, Show)

instance F.Storable B where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure B

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          B -> return ()

data C

data A = A
  { a_x :: FC.CDouble
  , a_label :: F.Ptr FC.CChar
  , a_samples :: (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar
  , a_b :: B
  , a_c :: F.Ptr C
  }
  deriving stock (Eq, Show)

instance F.Storable A where

  sizeOf = \_ -> (152 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure A
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (144 :: Int)
      <*> F.peekByteOff ptr0 (144 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 a_label3 a_samples4 a_b5 a_c6 ->
               F.pokeByteOff ptr0 (0 :: Int) a_x2
            >> F.pokeByteOff ptr0 (8 :: Int) a_label3
            >> F.pokeByteOff ptr0 (16 :: Int) a_samples4
            >> F.pokeByteOff ptr0 (144 :: Int) a_b5
            >> F.pokeByteOff ptr0 (144 :: Int) a_c6

{-| __from C:__ @resample@ -}
foreign import ccall safe "hs_bindgen_test_spec_examples_bab0544b0c2274da" resample_wrapper
  :: F.Ptr Int32_T
     {- ^ __from C:__ @res_m_num_valid_samples@ -}
  -> F.Ptr Cint16_T
     {- ^ __from C:__ @res_m_iq_int@ -}
  -> Int64_T
     {- ^ __from C:__ @res_m_old_rate@ -}
  -> Int64_T
     {- ^ __from C:__ @res_m_new_rate@ -}
  -> F.Ptr Cint16_T
     {- ^ __from C:__ @res_m_iq_resampled_int@ -}
  -> IO ()

resample :: (F.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ()
resample =
  \x0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          \x4 ->
            HsBindgen.Runtime.ConstantArray.withPtr x4 (\ptr5 ->
                                                          HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr6 ->
                                                                                                        resample_wrapper x0 ptr6 x2 x3 ptr5))
