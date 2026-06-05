{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/field_name_reuse_omit.h>"
  , "/* test_declarationsfield_name_reuse__Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2b628f8ea7c448cd (void)) (void)"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_declarationsfield_name_reuse__Example_get_bar@
foreign import ccall unsafe "hs_bindgen_2b628f8ea7c448cd" hs_bindgen_2b628f8ea7c448cd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_declarationsfield_name_reuse__Example_get_bar@
hs_bindgen_2b628f8ea7c448cd :: IO (RIP.FunPtr (IO ()))
hs_bindgen_2b628f8ea7c448cd =
  RIP.fromFFIType hs_bindgen_2b628f8ea7c448cd_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @declarations\/field_name_reuse_omit.h 28:6@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
bar :: RIP.FunPtr (IO ())
bar = RIP.unsafePerformIO hs_bindgen_2b628f8ea7c448cd
