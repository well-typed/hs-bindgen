{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/varargs.h>"
  , "/* test_functionsvarargs_Example_get_h_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1ee4c98815eaff8a (void)) (void)"
  , "{"
  , "  return &h;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1ee4c98815eaff8a" hs_bindgen_1ee4c98815eaff8a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionsvarargs_Example_get_h_ptr@
hs_bindgen_1ee4c98815eaff8a ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_1ee4c98815eaff8a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1ee4c98815eaff8a_base

{-# NOINLINE h_ptr #-}

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h:8:6@

    __exported by:__ @functions\/varargs.h@
-}
h_ptr :: Ptr.FunPtr (IO ())
h_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1ee4c98815eaff8a
