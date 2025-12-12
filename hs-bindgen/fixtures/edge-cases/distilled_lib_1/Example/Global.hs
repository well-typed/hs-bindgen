{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "/* test_edgecasesdistilled_lib_1_Example_get_v_ptr */"
  , "__attribute__ ((const))"
  , "var_t *hs_bindgen_9b325860ee78839e (void)"
  , "{"
  , "  return &v;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9b325860ee78839e" hs_bindgen_9b325860ee78839e_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_edgecasesdistilled_lib_1_Example_get_v_ptr@
hs_bindgen_9b325860ee78839e ::
     IO (Ptr.Ptr Var_t)
hs_bindgen_9b325860ee78839e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9b325860ee78839e_base

{-# NOINLINE v_ptr #-}

{-| __C declaration:__ @v@

    __defined at:__ @edge-cases\/distilled_lib_1.h:91:14@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
v_ptr :: Ptr.Ptr Var_t
v_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9b325860ee78839e
