{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.vector_alloc
    , Example.Unsafe.vector_free
    , Example.Unsafe.vector_reverse
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/flam_functions.h>"
  , "struct Vector *hs_bindgen_66fe57793f0712c2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (vector_alloc)(arg1);"
  , "}"
  , "void hs_bindgen_2fb197c3fb9a75a3 ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  (vector_free)(arg1);"
  , "}"
  , "void hs_bindgen_c3965610d4826d1a ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  (vector_reverse)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_alloc@
foreign import ccall unsafe "hs_bindgen_66fe57793f0712c2" hs_bindgen_66fe57793f0712c2_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_alloc@
hs_bindgen_66fe57793f0712c2 ::
     BG.CInt
  -> IO (BG.Ptr Vector)
hs_bindgen_66fe57793f0712c2 =
  BG.fromFFIType hs_bindgen_66fe57793f0712c2_base

{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc ::
     BG.CInt
     -- ^ __C declaration:__ @n@
  -> IO (BG.Ptr Vector)
vector_alloc = hs_bindgen_66fe57793f0712c2

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_free@
foreign import ccall unsafe "hs_bindgen_2fb197c3fb9a75a3" hs_bindgen_2fb197c3fb9a75a3_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_free@
hs_bindgen_2fb197c3fb9a75a3 ::
     BG.Ptr Vector
  -> IO ()
hs_bindgen_2fb197c3fb9a75a3 =
  BG.fromFFIType hs_bindgen_2fb197c3fb9a75a3_base

{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free ::
     BG.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_free = hs_bindgen_2fb197c3fb9a75a3

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_reverse@
foreign import ccall unsafe "hs_bindgen_c3965610d4826d1a" hs_bindgen_c3965610d4826d1a_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Unsafe_vector_reverse@
hs_bindgen_c3965610d4826d1a ::
     BG.Ptr Vector
  -> IO ()
hs_bindgen_c3965610d4826d1a =
  BG.fromFFIType hs_bindgen_c3965610d4826d1a_base

{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse ::
     BG.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_reverse = hs_bindgen_c3965610d4826d1a
