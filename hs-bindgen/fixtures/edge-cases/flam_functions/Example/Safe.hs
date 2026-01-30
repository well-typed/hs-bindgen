{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Example
import Prelude (IO)

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
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_alloc@
hs_bindgen_231473be98482b20 ::
     FC.CInt
  -> IO (Ptr.Ptr Vector)
hs_bindgen_231473be98482b20 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_231473be98482b20_base

{-| __C declaration:__ @vector_alloc@

    __defined at:__ @edge-cases\/flam_functions.h 6:16@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_alloc ::
     FC.CInt
     -- ^ __C declaration:__ @n@
  -> IO (Ptr.Ptr Vector)
vector_alloc = hs_bindgen_231473be98482b20

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
foreign import ccall safe "hs_bindgen_d3aecc79bd3c1993" hs_bindgen_d3aecc79bd3c1993_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_free@
hs_bindgen_d3aecc79bd3c1993 ::
     Ptr.Ptr Vector
  -> IO ()
hs_bindgen_d3aecc79bd3c1993 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d3aecc79bd3c1993_base

{-| __C declaration:__ @vector_free@

    __defined at:__ @edge-cases\/flam_functions.h 8:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_free ::
     Ptr.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_free = hs_bindgen_d3aecc79bd3c1993

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
foreign import ccall safe "hs_bindgen_bfc48cbf5e4cc2ca" hs_bindgen_bfc48cbf5e4cc2ca_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesflam_functions_Example_Safe_vector_reverse@
hs_bindgen_bfc48cbf5e4cc2ca ::
     Ptr.Ptr Vector
  -> IO ()
hs_bindgen_bfc48cbf5e4cc2ca =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bfc48cbf5e4cc2ca_base

{-| __C declaration:__ @vector_reverse@

    __defined at:__ @edge-cases\/flam_functions.h 10:6@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
vector_reverse ::
     Ptr.Ptr Vector
     -- ^ __C declaration:__ @v@
  -> IO ()
vector_reverse = hs_bindgen_bfc48cbf5e4cc2ca
