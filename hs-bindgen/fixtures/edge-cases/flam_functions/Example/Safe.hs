{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/flam_functions.h>"
  , "struct Vector *hs_bindgen_231473be98482b20 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return vector_alloc(arg1);"
  , "}"
  , "void hs_bindgen_d3aecc79bd3c1993 ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  vector_free(arg1);"
  , "}"
  , "void hs_bindgen_bfc48cbf5e4cc2ca ("
  , "  struct Vector *arg1"
  , ")"
  , "{"
  , "  vector_reverse(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_alloc@
foreign import ccall safe "hs_bindgen_231473be98482b20" hs_bindgen_231473be98482b20_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_alloc@
hs_bindgen_231473be98482b20 ::
     RIP.CInt
  -> IO (RIP.Ptr Vector)
hs_bindgen_231473be98482b20 =
  RIP.fromFFIType hs_bindgen_231473be98482b20_base

{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc ::
     RIP.CInt
     -- ^ __C declaration:__ @n@
  -> IO (RIP.Ptr Vector)
vector_alloc = hs_bindgen_231473be98482b20

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
foreign import ccall safe "hs_bindgen_d3aecc79bd3c1993" hs_bindgen_d3aecc79bd3c1993_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
hs_bindgen_d3aecc79bd3c1993 ::
     RIP.Ptr Vector
  -> IO ()
hs_bindgen_d3aecc79bd3c1993 =
  RIP.fromFFIType hs_bindgen_d3aecc79bd3c1993_base

{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free ::
     RIP.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_free = hs_bindgen_d3aecc79bd3c1993

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
foreign import ccall safe "hs_bindgen_bfc48cbf5e4cc2ca" hs_bindgen_bfc48cbf5e4cc2ca_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
hs_bindgen_bfc48cbf5e4cc2ca ::
     RIP.Ptr Vector
  -> IO ()
hs_bindgen_bfc48cbf5e4cc2ca =
  RIP.fromFFIType hs_bindgen_bfc48cbf5e4cc2ca_base

{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse ::
     RIP.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_reverse = hs_bindgen_bfc48cbf5e4cc2ca
