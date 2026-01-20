{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cp@
hs_bindgen_9445b7269de35e9e :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_9445b7269de35e9e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9445b7269de35e9e_base

{-# NOINLINE square_cp #-}
{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h 9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cp =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9445b7269de35e9e

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
foreign import ccall unsafe "hs_bindgen_b0c647124e93645d" hs_bindgen_b0c647124e93645d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
hs_bindgen_b0c647124e93645d :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_b0c647124e93645d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b0c647124e93645d_base

{-# NOINLINE square_pc #-}
{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h 11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pc =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b0c647124e93645d

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
foreign import ccall unsafe "hs_bindgen_87ee2f018e6d262a" hs_bindgen_87ee2f018e6d262a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
hs_bindgen_87ee2f018e6d262a :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_87ee2f018e6d262a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_87ee2f018e6d262a_base

{-# NOINLINE square_cc #-}
{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h 13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cc =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_87ee2f018e6d262a

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
foreign import ccall unsafe "hs_bindgen_0f31a17bf5ee4e4f" hs_bindgen_0f31a17bf5ee4e4f_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
hs_bindgen_0f31a17bf5ee4e4f :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_0f31a17bf5ee4e4f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0f31a17bf5ee4e4f_base

{-# NOINLINE square_pp #-}
{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h 15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pp =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0f31a17bf5ee4e4f
