{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/anon_multiple_typedefs.h>"
  , "void hs_bindgen_c97a0d4458699ad7 ("
  , "  point2a *arg1,"
  , "  point2b arg2"
  , ")"
  , "{"
  , "  (test)(*arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_Safe_test@
foreign import ccall safe "hs_bindgen_c97a0d4458699ad7" hs_bindgen_c97a0d4458699ad7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_Safe_test@
hs_bindgen_c97a0d4458699ad7 ::
     RIP.Ptr Point2a
  -> Point2b
  -> IO ()
hs_bindgen_c97a0d4458699ad7 =
  RIP.fromFFIType hs_bindgen_c97a0d4458699ad7_base

{-| __C declaration:__ @test@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 14:6@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
test ::
     Point2a
     -- ^ __C declaration:__ @x@
  -> Point2b
     -- ^ __C declaration:__ @y@
  -> IO ()
test =
  \x0 ->
    \y1 ->
      RIP.with x0 (\x2 ->
                     hs_bindgen_c97a0d4458699ad7 x2 y1)
