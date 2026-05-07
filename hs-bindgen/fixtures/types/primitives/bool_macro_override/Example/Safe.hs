{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    , Example.Safe.g
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/bool_macro_override.h>"
  , "void hs_bindgen_fc2c0275afbb3c2e ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  (f)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d07d93e6b7330d03 ("
  , "  A arg1,"
  , "  bool arg2"
  , ")"
  , "{"
  , "  (g)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Safe_f@
foreign import ccall safe "hs_bindgen_fc2c0275afbb3c2e" hs_bindgen_fc2c0275afbb3c2e_base ::
     RIP.Int32
  -> RIP.Word8
  -> IO ()

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Safe_f@
hs_bindgen_fc2c0275afbb3c2e ::
     A
  -> RIP.CBool
  -> IO ()
hs_bindgen_fc2c0275afbb3c2e =
  RIP.fromFFIType hs_bindgen_fc2c0275afbb3c2e_base

{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_macro_override.h 10:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> RIP.CBool
     -- ^ __C declaration:__ @y@
  -> IO ()
f = hs_bindgen_fc2c0275afbb3c2e

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Safe_g@
foreign import ccall safe "hs_bindgen_d07d93e6b7330d03" hs_bindgen_d07d93e6b7330d03_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_Safe_g@
hs_bindgen_d07d93e6b7330d03 ::
     A
  -> Bool'
  -> IO ()
hs_bindgen_d07d93e6b7330d03 =
  RIP.fromFFIType hs_bindgen_d07d93e6b7330d03_base

{-| __C declaration:__ @g@

    __defined at:__ @types\/primitives\/bool_macro_override.h 13:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
g ::
     A
     -- ^ __C declaration:__ @x@
  -> Bool'
     -- ^ __C declaration:__ @y@
  -> IO ()
g = hs_bindgen_d07d93e6b7330d03
