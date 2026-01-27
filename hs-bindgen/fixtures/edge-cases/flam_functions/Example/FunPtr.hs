{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_alloc@
hs_bindgen_d243fc6d495ec901 :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr Vector)))
hs_bindgen_d243fc6d495ec901 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d243fc6d495ec901_base

{-# NOINLINE vector_alloc #-}
{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr Vector))
vector_alloc =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d243fc6d495ec901

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_free@
foreign import ccall unsafe "hs_bindgen_1d7c878fb2029890" hs_bindgen_1d7c878fb2029890_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_free@
hs_bindgen_1d7c878fb2029890 :: IO (Ptr.FunPtr ((Ptr.Ptr Vector) -> IO ()))
hs_bindgen_1d7c878fb2029890 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1d7c878fb2029890_base

{-# NOINLINE vector_free #-}
{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free :: Ptr.FunPtr ((Ptr.Ptr Vector) -> IO ())
vector_free =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1d7c878fb2029890

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_reverse@
foreign import ccall unsafe "hs_bindgen_630caac5f56516fe" hs_bindgen_630caac5f56516fe_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesflam_functions_Example_get_vector_reverse@
hs_bindgen_630caac5f56516fe :: IO (Ptr.FunPtr ((Ptr.Ptr Vector) -> IO ()))
hs_bindgen_630caac5f56516fe =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_630caac5f56516fe_base

{-# NOINLINE vector_reverse #-}
{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse :: Ptr.FunPtr ((Ptr.Ptr Vector) -> IO ())
vector_reverse =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_630caac5f56516fe
