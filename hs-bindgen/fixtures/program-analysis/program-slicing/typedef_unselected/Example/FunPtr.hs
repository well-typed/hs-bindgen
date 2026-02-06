{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program-slicing/typedef_unselected.h>"
  , "/* test_programanalysisprogramslici_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e57577b970e09cca (void)) ("
  , "  U arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_e57577b970e09cca" hs_bindgen_e57577b970e09cca_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
hs_bindgen_e57577b970e09cca :: IO (Ptr.FunPtr (U -> IO ()))
hs_bindgen_e57577b970e09cca =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e57577b970e09cca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/typedef_unselected.h 12:6@

    __exported by:__ @program-analysis\/program-slicing\/typedef_unselected.h@
-}
bar :: Ptr.FunPtr (U -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e57577b970e09cca
