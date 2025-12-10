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
  , "/* test_declarationsdefinitions_Example_get_n_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_d0561a54a41cb140 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- | __unique:__ @test_declarationsdefinitions_Example_get_n_ptr@
foreign import ccall unsafe "hs_bindgen_d0561a54a41cb140" hs_bindgen_d0561a54a41cb140 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE n_ptr #-}

{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h:18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n_ptr :: Ptr.Ptr FC.CInt
n_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d0561a54a41cb140
