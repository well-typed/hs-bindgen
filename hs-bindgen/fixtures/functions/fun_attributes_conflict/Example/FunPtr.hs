{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "/* test_functionsfun_attributes_confl_Example_get_square_cp */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9445b7269de35e9e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cp;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_pc */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b0c647124e93645d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pc;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_cc */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_87ee2f018e6d262a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cc;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_pp */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0f31a17bf5ee4e4f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pp;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cp@
foreign import ccall unsafe "hs_bindgen_9445b7269de35e9e" hs_bindgen_9445b7269de35e9e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cp@
hs_bindgen_9445b7269de35e9e :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_9445b7269de35e9e =
  RIP.fromFFIType hs_bindgen_9445b7269de35e9e_base

{-# NOINLINE square_cp #-}
{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h 9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square_cp =
  RIP.unsafePerformIO hs_bindgen_9445b7269de35e9e

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
foreign import ccall unsafe "hs_bindgen_b0c647124e93645d" hs_bindgen_b0c647124e93645d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
hs_bindgen_b0c647124e93645d :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_b0c647124e93645d =
  RIP.fromFFIType hs_bindgen_b0c647124e93645d_base

{-# NOINLINE square_pc #-}
{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h 11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square_pc =
  RIP.unsafePerformIO hs_bindgen_b0c647124e93645d

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
foreign import ccall unsafe "hs_bindgen_87ee2f018e6d262a" hs_bindgen_87ee2f018e6d262a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
hs_bindgen_87ee2f018e6d262a :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_87ee2f018e6d262a =
  RIP.fromFFIType hs_bindgen_87ee2f018e6d262a_base

{-# NOINLINE square_cc #-}
{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h 13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square_cc =
  RIP.unsafePerformIO hs_bindgen_87ee2f018e6d262a

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
foreign import ccall unsafe "hs_bindgen_0f31a17bf5ee4e4f" hs_bindgen_0f31a17bf5ee4e4f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
hs_bindgen_0f31a17bf5ee4e4f :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_0f31a17bf5ee4e4f =
  RIP.fromFFIType hs_bindgen_0f31a17bf5ee4e4f_base

{-# NOINLINE square_pp #-}
{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h 15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square_pp =
  RIP.unsafePerformIO hs_bindgen_0f31a17bf5ee4e4f
