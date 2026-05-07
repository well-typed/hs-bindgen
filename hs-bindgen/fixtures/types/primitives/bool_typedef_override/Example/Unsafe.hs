{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/bool_typedef_override.h>"
  , "void hs_bindgen_1ab53e827ec0c8e2 ("
  , "  A arg1,"
  , "  bool arg2"
  , ")"
  , "{"
  , "  (f)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_typedef__Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_1ab53e827ec0c8e2" hs_bindgen_1ab53e827ec0c8e2_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesprimitivesbool_typedef__Example_Unsafe_f@
hs_bindgen_1ab53e827ec0c8e2 ::
     A
  -> Bool'
  -> IO ()
hs_bindgen_1ab53e827ec0c8e2 =
  RIP.fromFFIType hs_bindgen_1ab53e827ec0c8e2_base

{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_typedef_override.h 6:6@

    __exported by:__ @types\/primitives\/bool_typedef_override.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> Bool'
     -- ^ __C declaration:__ @y@
  -> IO ()
f = hs_bindgen_1ab53e827ec0c8e2
