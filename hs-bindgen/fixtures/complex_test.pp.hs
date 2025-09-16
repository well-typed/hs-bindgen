{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <complex_test.h>\nvoid hs_bindgen_test_complex_test_b84ea846e04d5fd6 (float _Complex *arg1, float _Complex *arg2, float _Complex *arg3) { *arg3 = multiply_complex_f(*arg1, *arg2); }\nvoid hs_bindgen_test_complex_test_8dd079d1707c36b3 (double _Complex *arg1, double _Complex *arg2, double _Complex *arg3) { *arg3 = add_complex(*arg1, *arg2); }\n/* get_multiply_complex_f_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_complex_test_a7d89c01385c8c56 (void)) (float _Complex arg1, float _Complex arg2) { return &multiply_complex_f; } \n/* get_add_complex_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_complex_test_b6226a5bde741b3f (void)) (double _Complex arg1, double _Complex arg2) { return &add_complex; } \n/* get_global_complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_complex_test_69e4d4972011967b (void) { return &global_complex_float; } \n/* get_global_complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_complex_test_c3633906ced5dab3 (void) { return &global_complex_double; } \n/* get_global_complex_float_flipped_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_complex_test_7ef41813e25ff8c1 (void) { return &global_complex_float_flipped; } \n/* get_global_complex_double_flipped_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_complex_test_abdd562bd1b14921 (void) { return &global_complex_double_flipped; } \n/* get_global_Complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_complex_test_02f701d4163d6ce7 (void) { return &global_Complex_float; } \n/* get_global_Complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_complex_test_a6117bb5e7cacd17 (void) { return &global_Complex_double; } \n/* get_global_Complex_float_flipped_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_complex_test_da2309480d364cee (void) { return &global_Complex_float_flipped; } \n/* get_global_Complex_double_flipped_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_complex_test_467427dc59fbef50 (void) { return &global_Complex_double_flipped; } \n/* get_const_complex_float_ptr */ __attribute__ ((const)) float _Complex const *hs_bindgen_test_complex_test_bb0fb18f3dfee47d (void) { return &const_complex_float; } \n/* get_const_complex_double_ptr */ __attribute__ ((const)) double _Complex const *hs_bindgen_test_complex_test_f491f52e529a459a (void) { return &const_complex_double; } \n/* get_volatile_complex_float_ptr */ __attribute__ ((const)) float _Complex *hs_bindgen_test_complex_test_ecb5f4a0ccb7ee75 (void) { return &volatile_complex_float; } \n/* get_volatile_complex_double_ptr */ __attribute__ ((const)) double _Complex *hs_bindgen_test_complex_test_6b136090c38a69c4 (void) { return &volatile_complex_double; } \n/* get_complex_float_array_ptr */ __attribute__ ((const)) float _Complex (*hs_bindgen_test_complex_test_55b7fb104be53f70 (void))[10] { return &complex_float_array; } \n/* get_complex_double_array_ptr */ __attribute__ ((const)) double _Complex (*hs_bindgen_test_complex_test_0b63f3bda9243457 (void))[10] { return &complex_double_array; } \n")

{-| __C declaration:__ @complex_object_t@

    __defined at:__ @complex_test.h:24:9@

    __exported by:__ @complex_test.h@
-}
data Complex_object_t = Complex_object_t
  { complex_object_t_velocity :: Data.Complex.Complex FC.CFloat
    {- ^ __C declaration:__ @velocity@

         __defined at:__ @complex_test.h:25:20@

         __exported by:__ @complex_test.h@
    -}
  , complex_object_t_position :: Data.Complex.Complex FC.CDouble
    {- ^ __C declaration:__ @position@

         __defined at:__ @complex_test.h:26:20@

         __exported by:__ @complex_test.h@
    -}
  , complex_object_t_id :: FC.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @complex_test.h:27:9@

         __exported by:__ @complex_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Complex_object_t where

  sizeOf = \_ -> (32 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Complex_object_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Complex_object_t
            complex_object_t_velocity2
            complex_object_t_position3
            complex_object_t_id4 ->
                 F.pokeByteOff ptr0 (0 :: Int) complex_object_t_velocity2
              >> F.pokeByteOff ptr0 (8 :: Int) complex_object_t_position3
              >> F.pokeByteOff ptr0 (24 :: Int) complex_object_t_id4

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @complex_test.h:21:16@

    __exported by:__ @complex_test.h@
-}
foreign import ccall safe "hs_bindgen_test_complex_test_b84ea846e04d5fd6" multiply_complex_f_wrapper
  :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
     {- ^ __C declaration:__ @a@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
     {- ^ __C declaration:__ @b@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

multiply_complex_f :: (Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat)
multiply_complex_f =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        multiply_complex_f_wrapper y3 y2 z4)))

{-| __C declaration:__ @add_complex@

    __defined at:__ @complex_test.h:22:16@

    __exported by:__ @complex_test.h@
-}
foreign import ccall safe "hs_bindgen_test_complex_test_8dd079d1707c36b3" add_complex_wrapper
  :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
     {- ^ __C declaration:__ @a@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
     {- ^ __C declaration:__ @b@
     -}
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

add_complex :: (Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble)
add_complex =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        add_complex_wrapper y3 y2 z4)))

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @complex_test.h:21:16@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_a7d89c01385c8c56" hs_bindgen_test_complex_test_a7d89c01385c8c56
  :: IO (Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE multiply_complex_f_ptr #-}

multiply_complex_f_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat))
multiply_complex_f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_a7d89c01385c8c56

{-| __C declaration:__ @add_complex@

    __defined at:__ @complex_test.h:22:16@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_b6226a5bde741b3f" hs_bindgen_test_complex_test_b6226a5bde741b3f
  :: IO (Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE add_complex_ptr #-}

add_complex_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble))
add_complex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_b6226a5bde741b3f

{-| __C declaration:__ @global_complex_float@

    __defined at:__ @complex_test.h:3:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_69e4d4972011967b" hs_bindgen_test_complex_test_69e4d4972011967b
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_ptr #-}

global_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_69e4d4972011967b

{-| __C declaration:__ @global_complex_double@

    __defined at:__ @complex_test.h:4:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_c3633906ced5dab3" hs_bindgen_test_complex_test_c3633906ced5dab3
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_ptr #-}

global_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_c3633906ced5dab3

{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @complex_test.h:6:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_7ef41813e25ff8c1" hs_bindgen_test_complex_test_7ef41813e25ff8c1
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_flipped_ptr #-}

global_complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_7ef41813e25ff8c1

{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @complex_test.h:7:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_abdd562bd1b14921" hs_bindgen_test_complex_test_abdd562bd1b14921
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_flipped_ptr #-}

global_complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_abdd562bd1b14921

{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @complex_test.h:9:24@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_02f701d4163d6ce7" hs_bindgen_test_complex_test_02f701d4163d6ce7
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_ptr #-}

global_Complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_02f701d4163d6ce7

{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @complex_test.h:10:24@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_a6117bb5e7cacd17" hs_bindgen_test_complex_test_a6117bb5e7cacd17
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_ptr #-}

global_Complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_a6117bb5e7cacd17

{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @complex_test.h:12:24@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_da2309480d364cee" hs_bindgen_test_complex_test_da2309480d364cee
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_flipped_ptr #-}

global_Complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_da2309480d364cee

{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @complex_test.h:13:24@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_467427dc59fbef50" hs_bindgen_test_complex_test_467427dc59fbef50
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_flipped_ptr #-}

global_Complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_467427dc59fbef50

{-| __C declaration:__ @const_complex_float@

    __defined at:__ @complex_test.h:15:29@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_bb0fb18f3dfee47d" hs_bindgen_test_complex_test_bb0fb18f3dfee47d
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE const_complex_float_ptr #-}

const_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
const_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_bb0fb18f3dfee47d

{-# NOINLINE const_complex_float #-}

const_complex_float :: Data.Complex.Complex FC.CFloat
const_complex_float =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_float_ptr)

{-| __C declaration:__ @const_complex_double@

    __defined at:__ @complex_test.h:16:29@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_f491f52e529a459a" hs_bindgen_test_complex_test_f491f52e529a459a
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE const_complex_double_ptr #-}

const_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
const_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_f491f52e529a459a

{-# NOINLINE const_complex_double #-}

const_complex_double :: Data.Complex.Complex FC.CDouble
const_complex_double =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_double_ptr)

{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @complex_test.h:18:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_ecb5f4a0ccb7ee75" hs_bindgen_test_complex_test_ecb5f4a0ccb7ee75
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE volatile_complex_float_ptr #-}

volatile_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
volatile_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_ecb5f4a0ccb7ee75

{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @complex_test.h:19:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_6b136090c38a69c4" hs_bindgen_test_complex_test_6b136090c38a69c4
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE volatile_complex_double_ptr #-}

volatile_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
volatile_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_6b136090c38a69c4

{-| __C declaration:__ @complex_float_array@

    __defined at:__ @complex_test.h:30:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_55b7fb104be53f70" hs_bindgen_test_complex_test_55b7fb104be53f70
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE complex_float_array_ptr #-}

complex_float_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat))
complex_float_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_55b7fb104be53f70

{-| __C declaration:__ @complex_double_array@

    __defined at:__ @complex_test.h:31:23@

    __exported by:__ @complex_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_test_0b63f3bda9243457" hs_bindgen_test_complex_test_0b63f3bda9243457
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE complex_double_array_ptr #-}

complex_double_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble))
complex_double_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_test_0b63f3bda9243457
