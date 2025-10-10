{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(HsBindgen.Runtime.Prelude.addCSource "#include <spec_examples.h>\nvoid hs_bindgen_test_spec_examples_7d4128962cfce15d (int32_T *arg1, cint16_T *arg2, int64_T arg3, int64_T arg4, cint16_T *arg5) { resample(arg1, arg2, arg3, arg4, arg5); }\n/* get_resample_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_spec_examples_46b04422dcd0bbd5 (void)) (int32_T *arg1, cint16_T arg2[30720000], int64_T arg3, int64_T arg4, cint16_T arg5[30720000]) { return &resample; } \n")

{-| Examples from the initial specification

__C declaration:__ @int16_T@

__defined at:__ @spec_examples.h:10:15@

__exported by:__ @spec_examples.h@
-}
newtype Int16_T = Int16_T
  { un_Int16_T :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @int32_T@

    __defined at:__ @spec_examples.h:11:13@

    __exported by:__ @spec_examples.h@
-}
newtype Int32_T = Int32_T
  { un_Int32_T :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @int64_T@

    __defined at:__ @spec_examples.h:12:19@

    __exported by:__ @spec_examples.h@
-}
newtype Int64_T = Int64_T
  { un_Int64_T :: FC.CLLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __defined at:__ @spec_examples.h:14:9@

    __exported by:__ @spec_examples.h@
-}
data Cint16_T = Cint16_T
  { cint16_T_re :: Int16_T
    {- ^ __C declaration:__ @re@

         __defined at:__ @spec_examples.h:15:11@

         __exported by:__ @spec_examples.h@
    -}
  , cint16_T_im :: Int16_T
    {- ^ __C declaration:__ @im@

         __defined at:__ @spec_examples.h:16:11@

         __exported by:__ @spec_examples.h@
    -}
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

{-| __C declaration:__ @B@

    __defined at:__ @spec_examples.h:19:8@

    __exported by:__ @spec_examples.h@
-}
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

{-| __C declaration:__ @A@

    __defined at:__ @spec_examples.h:23:8@

    __exported by:__ @spec_examples.h@
-}
data A = A
  { a_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @spec_examples.h:24:10@

         __exported by:__ @spec_examples.h@
    -}
  , a_label :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @label@

         __defined at:__ @spec_examples.h:25:9@

         __exported by:__ @spec_examples.h@
    -}
  , a_samples :: (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar
    {- ^ __C declaration:__ @samples@

         __defined at:__ @spec_examples.h:26:8@

         __exported by:__ @spec_examples.h@
    -}
  , a_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @spec_examples.h:27:12@

         __exported by:__ @spec_examples.h@
    -}
  , a_c :: Ptr.Ptr C
    {- ^ __C declaration:__ @c@

         __defined at:__ @spec_examples.h:28:13@

         __exported by:__ @spec_examples.h@
    -}
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

{-| __C declaration:__ @C@

    __defined at:__ @spec_examples.h:28:10@

    __exported by:__ @spec_examples.h@
-}
data C

{-| __C declaration:__ @resample@

    __defined at:__ @spec_examples.h:31:6@

    __exported by:__ @spec_examples.h@
-}
foreign import ccall safe "hs_bindgen_test_spec_examples_7d4128962cfce15d" resample
  :: Ptr.Ptr Int32_T
     {- ^ __C declaration:__ @res_m_num_valid_samples@
     -}
  -> Ptr.Ptr Cint16_T
     {- ^ __C declaration:__ @res_m_iq_int@
     -}
  -> Int64_T
     {- ^ __C declaration:__ @res_m_old_rate@
     -}
  -> Int64_T
     {- ^ __C declaration:__ @res_m_new_rate@
     -}
  -> Ptr.Ptr Cint16_T
     {- ^ __C declaration:__ @res_m_iq_resampled_int@
     -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_spec_examples_46b04422dcd0bbd5" hs_bindgen_test_spec_examples_46b04422dcd0bbd5
  :: IO (Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ()))

{-# NOINLINE resample_ptr #-}

{-| __C declaration:__ @resample@

    __defined at:__ @spec_examples.h:31:6@

    __exported by:__ @spec_examples.h@
-}
resample_ptr :: Ptr.FunPtr ((Ptr.Ptr Int32_T) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> Int64_T -> Int64_T -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 30720000) Cint16_T) -> IO ())
resample_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_spec_examples_46b04422dcd0bbd5
