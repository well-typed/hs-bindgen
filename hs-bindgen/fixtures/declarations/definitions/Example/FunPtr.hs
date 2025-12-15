{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/definitions.h>"
  , "/* test_declarationsdefinitions_Example_get_foo_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bc546207e3a9a16e (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bc546207e3a9a16e" hs_bindgen_bc546207e3a9a16e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_declarationsdefinitions_Example_get_foo_ptr@
hs_bindgen_bc546207e3a9a16e ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CInt))
hs_bindgen_bc546207e3a9a16e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bc546207e3a9a16e_base

{-# NOINLINE foo_ptr #-}

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h:13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CInt)
foo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bc546207e3a9a16e
