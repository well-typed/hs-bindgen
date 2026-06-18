{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.globalVar
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_globalVar@
hs_bindgen_a370a6faf1531caa :: IO (RIP.Ptr (CA.ConstantArray 3 (RIP.FunPtr (A -> IO ()))))
hs_bindgen_a370a6faf1531caa =
  RIP.fromFFIType hs_bindgen_a370a6faf1531caa_base

{-# NOINLINE globalVar #-}
{-| __C declaration:__ @globalVar@

    __defined at:__ @functions\/callbacks.h 103:15@

    __exported by:__ @functions\/callbacks.h@
-}
globalVar :: RIP.Ptr (CA.ConstantArray 3 (RIP.FunPtr (A -> IO ())))
globalVar =
  RIP.unsafePerformIO hs_bindgen_a370a6faf1531caa
