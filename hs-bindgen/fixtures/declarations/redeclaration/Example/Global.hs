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
  [ "#include <declarations/redeclaration.h>"
  , "/* test_declarationsredeclaration_Example_get_x */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_6f47e5cbb92690b9 (void)"
  , "{"
  , "  return &x;"
  , "}"
  ]))

-- __unique:__ @test_declarationsredeclaration_Example_get_x@
foreign import ccall unsafe "hs_bindgen_6f47e5cbb92690b9" hs_bindgen_6f47e5cbb92690b9_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_declarationsredeclaration_Example_get_x@
hs_bindgen_6f47e5cbb92690b9 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_6f47e5cbb92690b9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6f47e5cbb92690b9_base

{-# NOINLINE x #-}
{-| __C declaration:__ @x@

    __defined at:__ @declarations\/redeclaration.h 11:5@

    __exported by:__ @declarations\/redeclaration.h@
-}
x :: Ptr.Ptr FC.CInt
x =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6f47e5cbb92690b9
