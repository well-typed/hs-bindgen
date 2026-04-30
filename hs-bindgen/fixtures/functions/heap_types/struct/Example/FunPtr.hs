{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fun
    , Example.FunPtr.param_underscore
    , Example.FunPtr.param_uppercase
    , Example.FunPtr.param_undersore_capital
    , Example.FunPtr.param_haskell_reserved_name
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "/* test_functionsheap_typesstruct_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_071e2eda58051e4a (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  , "/* test_functionsheap_typesstruct_Example_get_param_underscore */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ee52ac50697405f8 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_underscore;"
  , "}"
  , "/* test_functionsheap_typesstruct_Example_get_param_uppercase */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b51cac34e6c4eca3 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_uppercase;"
  , "}"
  , "/* test_functionsheap_typesstruct_Example_get_param_undersore_capital */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2302284a38d84764 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_undersore_capital;"
  , "}"
  , "/* test_functionsheap_typesstruct_Example_get_param_haskell_reserved_name */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_af57be8569d5f651 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &param_haskell_reserved_name;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_071e2eda58051e4a" hs_bindgen_071e2eda58051e4a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_fun@
hs_bindgen_071e2eda58051e4a :: IO (RIP.FunPtr (T -> IO T))
hs_bindgen_071e2eda58051e4a =
  RIP.fromFFIType hs_bindgen_071e2eda58051e4a_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct.h 9:3@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
fun :: RIP.FunPtr (T -> IO T)
fun = RIP.unsafePerformIO hs_bindgen_071e2eda58051e4a

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_underscore@
foreign import ccall unsafe "hs_bindgen_ee52ac50697405f8" hs_bindgen_ee52ac50697405f8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_underscore@
hs_bindgen_ee52ac50697405f8 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_ee52ac50697405f8 =
  RIP.fromFFIType hs_bindgen_ee52ac50697405f8_base

{-# NOINLINE param_underscore #-}
{-| __C declaration:__ @param_underscore@

    __defined at:__ @functions\/heap_types\/struct.h 12:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_underscore :: RIP.FunPtr (T -> IO ())
param_underscore =
  RIP.unsafePerformIO hs_bindgen_ee52ac50697405f8

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_uppercase@
foreign import ccall unsafe "hs_bindgen_b51cac34e6c4eca3" hs_bindgen_b51cac34e6c4eca3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_uppercase@
hs_bindgen_b51cac34e6c4eca3 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_b51cac34e6c4eca3 =
  RIP.fromFFIType hs_bindgen_b51cac34e6c4eca3_base

{-# NOINLINE param_uppercase #-}
{-| __C declaration:__ @param_uppercase@

    __defined at:__ @functions\/heap_types\/struct.h 13:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_uppercase :: RIP.FunPtr (T -> IO ())
param_uppercase =
  RIP.unsafePerformIO hs_bindgen_b51cac34e6c4eca3

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_undersore_capital@
foreign import ccall unsafe "hs_bindgen_2302284a38d84764" hs_bindgen_2302284a38d84764_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_undersore_capital@
hs_bindgen_2302284a38d84764 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_2302284a38d84764 =
  RIP.fromFFIType hs_bindgen_2302284a38d84764_base

{-# NOINLINE param_undersore_capital #-}
{-| __C declaration:__ @param_undersore_capital@

    __defined at:__ @functions\/heap_types\/struct.h 14:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_undersore_capital :: RIP.FunPtr (T -> IO ())
param_undersore_capital =
  RIP.unsafePerformIO hs_bindgen_2302284a38d84764

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_haskell_reserved_name@
foreign import ccall unsafe "hs_bindgen_af57be8569d5f651" hs_bindgen_af57be8569d5f651_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_param_haskell_reserved_name@
hs_bindgen_af57be8569d5f651 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_af57be8569d5f651 =
  RIP.fromFFIType hs_bindgen_af57be8569d5f651_base

{-# NOINLINE param_haskell_reserved_name #-}
{-| __C declaration:__ @param_haskell_reserved_name@

    __defined at:__ @functions\/heap_types\/struct.h 15:6@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
param_haskell_reserved_name :: RIP.FunPtr (T -> IO ())
param_haskell_reserved_name =
  RIP.unsafePerformIO hs_bindgen_af57be8569d5f651
