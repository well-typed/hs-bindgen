{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.test
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/unnamed_multiple_typedefs.h>"
  , "/* test_edgecasesunnamed_multiple_ty_Example_get_test */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5c204ec0a2f1c3e3 (void)) ("
  , "  point2a arg1,"
  , "  point2b arg2"
  , ")"
  , "{"
  , "  return &test;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesunnamed_multiple_ty_Example_get_test@
foreign import ccall unsafe "hs_bindgen_5c204ec0a2f1c3e3" hs_bindgen_5c204ec0a2f1c3e3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesunnamed_multiple_ty_Example_get_test@
hs_bindgen_5c204ec0a2f1c3e3 :: IO (BG.FunPtr (Point2a -> Point2b -> IO ()))
hs_bindgen_5c204ec0a2f1c3e3 =
  BG.fromFFIType hs_bindgen_5c204ec0a2f1c3e3_base

{-# NOINLINE test #-}
{-| __C declaration:__ @test@

    __defined at:__ @edge-cases\/unnamed_multiple_typedefs.h 14:6@

    __exported by:__ @edge-cases\/unnamed_multiple_typedefs.h@
-}
test :: BG.FunPtr (Point2a -> Point2b -> IO ())
test = BG.unsafePerformIO hs_bindgen_5c204ec0a2f1c3e3
