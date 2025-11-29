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
  , "/* ExampleNothingget_i_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e (void)"
  , "{"
  , "  return &i;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_i_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e" hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i_ptr #-}

{-| __C declaration:__ @i@

    __defined at:__ @functions\/fun_attributes.h:132:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
i_ptr :: Ptr.Ptr FC.CInt
i_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_cdc30ae5fb72cd6e
