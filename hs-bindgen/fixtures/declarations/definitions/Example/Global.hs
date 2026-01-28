{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "/* test_declarationsdefinitions_Example_get_n */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_417f0d4479c97357 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_get_n@
foreign import ccall unsafe "hs_bindgen_417f0d4479c97357" hs_bindgen_417f0d4479c97357_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_declarationsdefinitions_Example_get_n@
hs_bindgen_417f0d4479c97357 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_417f0d4479c97357 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_417f0d4479c97357_base

{-# NOINLINE n #-}
{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h 18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n :: Ptr.Ptr FC.CInt
n =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_417f0d4479c97357
