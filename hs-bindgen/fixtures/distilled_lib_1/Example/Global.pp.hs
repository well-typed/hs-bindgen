{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <distilled_lib_1.h>"
  , "/* get_v_ptr */"
  , "__attribute__ ((const))"
  , "var_t *hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6 (void)"
  , "{"
  , "  return &v;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6" hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6 ::
     IO (Ptr.Ptr Var_t)

{-# NOINLINE v_ptr #-}

{-| __C declaration:__ @v@

    __defined at:__ @distilled_lib_1.h:91:14@

    __exported by:__ @distilled_lib_1.h@
-}
v_ptr :: Ptr.Ptr Var_t
v_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6
