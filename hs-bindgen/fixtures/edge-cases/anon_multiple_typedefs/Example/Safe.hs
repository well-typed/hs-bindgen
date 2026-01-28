{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/anon_multiple_typedefs.h>"
  , "void hs_bindgen_c97a0d4458699ad7 ("
  , "  point2a *arg1,"
  , "  point2b arg2"
  , ")"
  , "{"
  , "  test(*arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_Safe_test@
foreign import ccall safe "hs_bindgen_c97a0d4458699ad7" hs_bindgen_c97a0d4458699ad7_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_Safe_test@
hs_bindgen_c97a0d4458699ad7 ::
     Ptr.Ptr Point2a
  -> Point2b
  -> IO ()
hs_bindgen_c97a0d4458699ad7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c97a0d4458699ad7_base

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
      F.with x0 (\x2 -> hs_bindgen_c97a0d4458699ad7 x2 y1)
