{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <skip_over_long_double.h>\nvoid hs_bindgen_test_skip_over_long_double_c7f5e756cd95b3ed (signed int arg1) { fun2(arg1); }\n/* get_fun2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_skip_over_long_double_94047676e402a0bf (void)) (signed int arg1) { return &fun2; } \n")

{-| __from C:__ @fun2@ -}
foreign import ccall safe "hs_bindgen_test_skip_over_long_double_c7f5e756cd95b3ed" fun2
  :: FC.CInt
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_skip_over_long_double_94047676e402a0bf" hs_bindgen_test_skip_over_long_double_94047676e402a0bf
  :: IO (F.FunPtr (FC.CInt -> IO ()))

{-# NOINLINE fun2_ptr #-}

fun2_ptr :: F.FunPtr (FC.CInt -> IO ())
fun2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_skip_over_long_double_94047676e402a0bf

data Struct2 = Struct2
  { struct2_x :: FC.CInt
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
          Struct2 struct2_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_x2
