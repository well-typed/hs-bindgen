{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/flam.h>"
  , "/* test_edgecasesflam_Example_get_reverse */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_153dbcd1cdfc13ae (void)) ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return &reverse;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesflam_Example_get_reverse@
foreign import ccall unsafe "hs_bindgen_153dbcd1cdfc13ae" hs_bindgen_153dbcd1cdfc13ae ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt))

{-# NOINLINE reverse #-}
{-| __C declaration:__ @reverse@

    __defined at:__ @edge-cases\/flam.h 36:12@

    __exported by:__ @edge-cases\/flam.h@
-}
reverse :: Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt)
reverse =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_153dbcd1cdfc13ae
