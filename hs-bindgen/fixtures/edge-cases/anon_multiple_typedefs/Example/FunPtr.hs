{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/anon_multiple_typedefs.h>"
  , "/* test_edgecasesanon_multiple_typed_Example_get_test */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8361517d92bfbc76 (void)) ("
  , "  point2a arg1,"
  , "  point2b arg2"
  , ")"
  , "{"
  , "  return &test;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_get_test@
foreign import ccall unsafe "hs_bindgen_8361517d92bfbc76" hs_bindgen_8361517d92bfbc76_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_get_test@
hs_bindgen_8361517d92bfbc76 :: IO (RIP.FunPtr (Point2a -> Point2b -> IO ()))
hs_bindgen_8361517d92bfbc76 =
  RIP.fromFFIType hs_bindgen_8361517d92bfbc76_base

{-# NOINLINE test #-}
{-| __C declaration:__ @test@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 14:6@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
test :: RIP.FunPtr (Point2a -> Point2b -> IO ())
test =
  RIP.unsafePerformIO hs_bindgen_8361517d92bfbc76
