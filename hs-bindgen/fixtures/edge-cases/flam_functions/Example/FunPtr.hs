{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/flam_functions.h>"
  , "/* test_edgecasesflam_functions_Example_get_vector_alloc */"
  , "__attribute__ ((const))"
  , "struct Vector *(*hs_bindgen_d243fc6d495ec901 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &vector_alloc;"
  , "}"
  , "/* test_edgecasesflam_functions_Example_get_vector_free */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1d7c878fb2029890 (void)) ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  return &vector_free;"
  , "}"
  , "/* test_edgecasesflam_functions_Example_get_vector_reverse */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_630caac5f56516fe (void)) ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  return &vector_reverse;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_alloc@
foreign import ccall unsafe "hs_bindgen_d243fc6d495ec901" hs_bindgen_d243fc6d495ec901_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_alloc@
hs_bindgen_d243fc6d495ec901 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr Vector)))
hs_bindgen_d243fc6d495ec901 =
  RIP.fromFFIType hs_bindgen_d243fc6d495ec901_base

{-# NOINLINE vector_alloc #-}
{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr Vector))
vector_alloc =
  RIP.unsafePerformIO hs_bindgen_d243fc6d495ec901

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_free@
foreign import ccall unsafe "hs_bindgen_1d7c878fb2029890" hs_bindgen_1d7c878fb2029890_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_free@
hs_bindgen_1d7c878fb2029890 :: IO (RIP.FunPtr ((RIP.Ptr Vector) -> IO ()))
hs_bindgen_1d7c878fb2029890 =
  RIP.fromFFIType hs_bindgen_1d7c878fb2029890_base

{-# NOINLINE vector_free #-}
{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free :: RIP.FunPtr ((RIP.Ptr Vector) -> IO ())
vector_free =
  RIP.unsafePerformIO hs_bindgen_1d7c878fb2029890

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_reverse@
foreign import ccall unsafe "hs_bindgen_630caac5f56516fe" hs_bindgen_630caac5f56516fe_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_reverse@
hs_bindgen_630caac5f56516fe :: IO (RIP.FunPtr ((RIP.Ptr Vector) -> IO ()))
hs_bindgen_630caac5f56516fe =
  RIP.fromFFIType hs_bindgen_630caac5f56516fe_base

{-# NOINLINE vector_reverse #-}
{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse :: RIP.FunPtr ((RIP.Ptr Vector) -> IO ())
vector_reverse =
  RIP.unsafePerformIO hs_bindgen_630caac5f56516fe
