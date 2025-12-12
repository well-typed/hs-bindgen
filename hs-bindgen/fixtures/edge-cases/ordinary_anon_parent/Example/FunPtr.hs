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
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "/* test_edgecasesordinary_anon_paren_Example_get__acos_ptr */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_bca479e11d297ab8 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &_acos;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bca479e11d297ab8" hs_bindgen_bca479e11d297ab8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_edgecasesordinary_anon_paren_Example_get__acos_ptr@
hs_bindgen_bca479e11d297ab8 ::
     IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))
hs_bindgen_bca479e11d297ab8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bca479e11d297ab8_base

{-# NOINLINE _acos_ptr #-}

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h:4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos_ptr :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
_acos_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bca479e11d297ab8
