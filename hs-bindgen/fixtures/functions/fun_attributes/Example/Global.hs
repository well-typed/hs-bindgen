{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* test_functionsfun_attributes_Example_get_i */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_cd32cb4982dd2d1a (void)"
  , "{"
  , "  return &i;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_Example_get_i@
foreign import ccall unsafe "hs_bindgen_cd32cb4982dd2d1a" hs_bindgen_cd32cb4982dd2d1a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i #-}

{-| __C declaration:__ @i@

    __defined at:__ @functions\/fun_attributes.h:132:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
i :: Ptr.Ptr FC.CInt
i =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cd32cb4982dd2d1a
