{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "/* test_declarationsdefinitions_Example_get_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8bb82cd68ce8efe4 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8bb82cd68ce8efe4" hs_bindgen_8bb82cd68ce8efe4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_declarationsdefinitions_Example_get_foo@
hs_bindgen_8bb82cd68ce8efe4 :: IO (BG.FunPtr (BG.CDouble -> IO BG.CInt))
hs_bindgen_8bb82cd68ce8efe4 =
  BG.fromFFIType hs_bindgen_8bb82cd68ce8efe4_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo :: BG.FunPtr (BG.CDouble -> IO BG.CInt)
foo = BG.unsafePerformIO hs_bindgen_8bb82cd68ce8efe4
