{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/anon_multiple_typedefs.h>"
  , "/* test_edgecasesanon_multiple_typed_Example_get_test */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8361517d92bfbc76 (void)) ("
  , "  point2a arg1,"
  , "  point2b arg2"
  , ")"
  , "{"
  , "  return &test;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_get_test@
foreign import ccall unsafe "hs_bindgen_8361517d92bfbc76" hs_bindgen_8361517d92bfbc76_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesanon_multiple_typed_Example_get_test@
hs_bindgen_8361517d92bfbc76 :: IO (Ptr.FunPtr (Point2a -> Point2b -> IO ()))
hs_bindgen_8361517d92bfbc76 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8361517d92bfbc76_base

{-# NOINLINE test #-}
{-| __C declaration:__ @test@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 14:6@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
test :: Ptr.FunPtr (Point2a -> Point2b -> IO ())
test =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8361517d92bfbc76
