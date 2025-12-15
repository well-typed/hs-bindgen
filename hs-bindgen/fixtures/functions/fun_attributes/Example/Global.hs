{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* test_functionsfun_attributes_Example_get_i_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_ea890ba2b1b3e0a8 (void)"
  , "{"
  , "  return &i;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ea890ba2b1b3e0a8" hs_bindgen_ea890ba2b1b3e0a8_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_functionsfun_attributes_Example_get_i_ptr@
hs_bindgen_ea890ba2b1b3e0a8 ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_ea890ba2b1b3e0a8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ea890ba2b1b3e0a8_base

{-# NOINLINE i_ptr #-}

{-| __C declaration:__ @i@

    __defined at:__ @functions\/fun_attributes.h:132:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
i_ptr :: Ptr.Ptr FC.CInt
i_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ea890ba2b1b3e0a8
