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
  , "/* test_declarationsdefinitions_Example_get_n */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_417f0d4479c97357 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- | __unique:__ @test_declarationsdefinitions_Example_get_n@
foreign import ccall unsafe "hs_bindgen_417f0d4479c97357" hs_bindgen_417f0d4479c97357 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE n #-}

{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h:18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n :: Ptr.Ptr FC.CInt
n =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_417f0d4479c97357
