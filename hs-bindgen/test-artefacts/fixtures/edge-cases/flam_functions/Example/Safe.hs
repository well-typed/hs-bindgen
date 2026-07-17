{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.vector_alloc
    , Example.Safe.vector_free
    , Example.Safe.vector_reverse
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/flam_functions.h>"
  , "struct Vector *hs_bindgen_231473be98482b20 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (vector_alloc)(arg1);"
  , "}"
  , "void hs_bindgen_d3aecc79bd3c1993 ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  (vector_free)(arg1);"
  , "}"
  , "void hs_bindgen_bfc48cbf5e4cc2ca ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  (vector_reverse)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_alloc@
foreign import ccall safe "hs_bindgen_231473be98482b20" hs_bindgen_231473be98482b20_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_alloc@
hs_bindgen_231473be98482b20 ::
     BG.CInt
  -> IO (BG.Ptr Vector)
hs_bindgen_231473be98482b20 =
  BG.fromFFIType hs_bindgen_231473be98482b20_base

{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc ::
     BG.CInt
     -- ^ __C declaration:__ @n@
  -> IO (BG.Ptr Vector)
vector_alloc = hs_bindgen_231473be98482b20

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
foreign import ccall safe "hs_bindgen_d3aecc79bd3c1993" hs_bindgen_d3aecc79bd3c1993_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
hs_bindgen_d3aecc79bd3c1993 ::
     BG.Ptr Vector
  -> IO ()
hs_bindgen_d3aecc79bd3c1993 =
  BG.fromFFIType hs_bindgen_d3aecc79bd3c1993_base

{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free ::
     BG.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_free = hs_bindgen_d3aecc79bd3c1993

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
foreign import ccall safe "hs_bindgen_bfc48cbf5e4cc2ca" hs_bindgen_bfc48cbf5e4cc2ca_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
hs_bindgen_bfc48cbf5e4cc2ca ::
     BG.Ptr Vector
  -> IO ()
hs_bindgen_bfc48cbf5e4cc2ca =
  BG.fromFFIType hs_bindgen_bfc48cbf5e4cc2ca_base

{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse ::
     BG.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_reverse = hs_bindgen_bfc48cbf5e4cc2ca
