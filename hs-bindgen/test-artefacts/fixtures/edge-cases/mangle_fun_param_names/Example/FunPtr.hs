{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.param_underscore
    , Example.FunPtr.param_uppercase
    , Example.FunPtr.param_undersore_capital
    , Example.FunPtr.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/mangle_fun_param_names.h>"
  , "/* test_edgecasesmangle_fun_param_na_Example_get_param_underscore */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e4a9e3fbd884275 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_underscore;"
  , "}"
  , "/* test_edgecasesmangle_fun_param_na_Example_get_param_uppercase */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3364ddfa127443d9 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_uppercase;"
  , "}"
  , "/* test_edgecasesmangle_fun_param_na_Example_get_param_undersore_capital */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_33f2dcd4eac093b3 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_undersore_capital;"
  , "}"
  , "/* test_edgecasesmangle_fun_param_na_Example_get_param_haskell_reserved_name */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_89c62efefee0f3b3 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_haskell_reserved_name;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_underscore@
foreign import ccall unsafe "hs_bindgen_2e4a9e3fbd884275" hs_bindgen_2e4a9e3fbd884275_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_underscore@
hs_bindgen_2e4a9e3fbd884275 :: IO (BG.FunPtr (T -> IO ()))
hs_bindgen_2e4a9e3fbd884275 =
  BG.fromFFIType hs_bindgen_2e4a9e3fbd884275_base

{-# NOINLINE param_underscore #-}
{-| __C declaration:__ @param_underscore@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 9:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_underscore :: BG.FunPtr (T -> IO ())
param_underscore =
  BG.unsafePerformIO hs_bindgen_2e4a9e3fbd884275

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_uppercase@
foreign import ccall unsafe "hs_bindgen_3364ddfa127443d9" hs_bindgen_3364ddfa127443d9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_uppercase@
hs_bindgen_3364ddfa127443d9 :: IO (BG.FunPtr (T -> IO ()))
hs_bindgen_3364ddfa127443d9 =
  BG.fromFFIType hs_bindgen_3364ddfa127443d9_base

{-# NOINLINE param_uppercase #-}
{-| __C declaration:__ @param_uppercase@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 10:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_uppercase :: BG.FunPtr (T -> IO ())
param_uppercase =
  BG.unsafePerformIO hs_bindgen_3364ddfa127443d9

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_undersore_capital@
foreign import ccall unsafe "hs_bindgen_33f2dcd4eac093b3" hs_bindgen_33f2dcd4eac093b3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_undersore_capital@
hs_bindgen_33f2dcd4eac093b3 :: IO (BG.FunPtr (T -> IO ()))
hs_bindgen_33f2dcd4eac093b3 =
  BG.fromFFIType hs_bindgen_33f2dcd4eac093b3_base

{-# NOINLINE param_undersore_capital #-}
{-| __C declaration:__ @param_undersore_capital@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 11:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_undersore_capital :: BG.FunPtr (T -> IO ())
param_undersore_capital =
  BG.unsafePerformIO hs_bindgen_33f2dcd4eac093b3

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_haskell_reserved_name@
foreign import ccall unsafe "hs_bindgen_89c62efefee0f3b3" hs_bindgen_89c62efefee0f3b3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesmangle_fun_param_na_Example_get_param_haskell_reserved_name@
hs_bindgen_89c62efefee0f3b3 :: IO (BG.FunPtr (T -> IO ()))
hs_bindgen_89c62efefee0f3b3 =
  BG.fromFFIType hs_bindgen_89c62efefee0f3b3_base

{-# NOINLINE param_haskell_reserved_name #-}
{-| __C declaration:__ @param_haskell_reserved_name@

    __defined at:__ @edge-cases\/mangle_fun_param_names.h 12:6@

    __exported by:__ @edge-cases\/mangle_fun_param_names.h@
-}
param_haskell_reserved_name :: BG.FunPtr (T -> IO ())
param_haskell_reserved_name =
  BG.unsafePerformIO hs_bindgen_89c62efefee0f3b3
