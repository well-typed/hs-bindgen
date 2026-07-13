{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.square_cp
    , Example.FunPtr.square_pc
    , Example.FunPtr.square_cc
    , Example.FunPtr.square_pp
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cp@
hs_bindgen_9445b7269de35e9e :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_9445b7269de35e9e =
  BG.fromFFIType hs_bindgen_9445b7269de35e9e_base

{-# NOINLINE square_cp #-}
{-| __C declaration:__ @square_cp@

    __defined at:__ @functions\/fun_attributes_conflict.h 9:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp :: BG.FunPtr (BG.CInt -> IO BG.CInt)
square_cp =
  BG.unsafePerformIO hs_bindgen_9445b7269de35e9e

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
foreign import ccall unsafe "hs_bindgen_b0c647124e93645d" hs_bindgen_b0c647124e93645d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc@
hs_bindgen_b0c647124e93645d :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_b0c647124e93645d =
  BG.fromFFIType hs_bindgen_b0c647124e93645d_base

{-# NOINLINE square_pc #-}
{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h 11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc :: BG.FunPtr (BG.CInt -> IO BG.CInt)
square_pc =
  BG.unsafePerformIO hs_bindgen_b0c647124e93645d

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
foreign import ccall unsafe "hs_bindgen_87ee2f018e6d262a" hs_bindgen_87ee2f018e6d262a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc@
hs_bindgen_87ee2f018e6d262a :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_87ee2f018e6d262a =
  BG.fromFFIType hs_bindgen_87ee2f018e6d262a_base

{-# NOINLINE square_cc #-}
{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h 13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc :: BG.FunPtr (BG.CInt -> IO BG.CInt)
square_cc =
  BG.unsafePerformIO hs_bindgen_87ee2f018e6d262a

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
foreign import ccall unsafe "hs_bindgen_0f31a17bf5ee4e4f" hs_bindgen_0f31a17bf5ee4e4f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp@
hs_bindgen_0f31a17bf5ee4e4f :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_0f31a17bf5ee4e4f =
  BG.fromFFIType hs_bindgen_0f31a17bf5ee4e4f_base

{-# NOINLINE square_pp #-}
{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h 15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp :: BG.FunPtr (BG.CInt -> IO BG.CInt)
square_pp =
  BG.unsafePerformIO hs_bindgen_0f31a17bf5ee4e4f
