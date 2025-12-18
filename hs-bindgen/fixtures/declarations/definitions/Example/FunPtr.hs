{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/definitions.h>"
  , "/* test_declarationsdefinitions_Example_get_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8bb82cd68ce8efe4 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- | __unique:__ @test_declarationsdefinitions_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8bb82cd68ce8efe4" hs_bindgen_8bb82cd68ce8efe4 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CInt))

{-# NOINLINE foo #-}

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h:13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo :: Ptr.FunPtr (FC.CDouble -> IO FC.CInt)
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8bb82cd68ce8efe4
