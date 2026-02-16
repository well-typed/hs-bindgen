{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_declarationsdefinitions_Example_get_foo@
hs_bindgen_8bb82cd68ce8efe4 :: IO (RIP.FunPtr (RIP.CDouble -> IO RIP.CInt))
hs_bindgen_8bb82cd68ce8efe4 =
  RIP.fromFFIType hs_bindgen_8bb82cd68ce8efe4_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo :: RIP.FunPtr (RIP.CDouble -> IO RIP.CInt)
foo = RIP.unsafePerformIO hs_bindgen_8bb82cd68ce8efe4
