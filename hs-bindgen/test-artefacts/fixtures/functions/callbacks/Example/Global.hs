{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.globalVar
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/callbacks.h>"
  , "/* test_functionscallbacks_Example_get_globalVar */"
  , "__attribute__ ((const))"
  , "void (*(*hs_bindgen_a370a6faf1531caa (void))[3]) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &globalVar;"
  , "}"
  ]))

-- __unique:__ @test_functionscallbacks_Example_get_globalVar@
foreign import ccall unsafe "hs_bindgen_a370a6faf1531caa" hs_bindgen_a370a6faf1531caa_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionscallbacks_Example_get_globalVar@
hs_bindgen_a370a6faf1531caa :: IO (BG.Ptr (CA.ConstantArray 3 (BG.FunPtr (A -> IO ()))))
hs_bindgen_a370a6faf1531caa =
  BG.fromFFIType hs_bindgen_a370a6faf1531caa_base

{-# NOINLINE globalVar #-}
{-| __C declaration:__ @globalVar@

    __defined at:__ @functions\/callbacks.h 103:15@

    __exported by:__ @functions\/callbacks.h@
-}
globalVar :: BG.Ptr (CA.ConstantArray 3 (BG.FunPtr (A -> IO ())))
globalVar =
  BG.unsafePerformIO hs_bindgen_a370a6faf1531caa
