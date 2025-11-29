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
  [ "#include <declarations/definitions.h>"
  , "/* Example_get_n_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationsdefinitions_bbd39fd4bdb7f276 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

{-| __unique:__ @Example_get_n_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsdefinitions_bbd39fd4bdb7f276" hs_bindgen_test_declarationsdefinitions_bbd39fd4bdb7f276 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE n_ptr #-}

{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h:18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n_ptr :: Ptr.Ptr FC.CInt
n_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationsdefinitions_bbd39fd4bdb7f276
