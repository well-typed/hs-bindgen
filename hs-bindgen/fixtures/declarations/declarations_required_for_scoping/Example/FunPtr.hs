{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "/* test_declarationsdeclarations_requ_Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0d4da37880af2263 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_declarationsdeclarations_requ_Example_get_f@
foreign import ccall unsafe "hs_bindgen_0d4da37880af2263" hs_bindgen_0d4da37880af2263_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_declarationsdeclarations_requ_Example_get_f@
hs_bindgen_0d4da37880af2263 :: IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_0d4da37880af2263 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0d4da37880af2263_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f :: Ptr.FunPtr (A -> IO ())
f =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0d4da37880af2263
