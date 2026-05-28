{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.g1
    , Example.Global.g2
    , Example.Global.g3
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/nesting/union_in_variable.h>"
  , "/* test_macrosreparsenestingunion_i_Example_get_G1 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_4b01ee4d60407c60 (void)"
  , "{"
  , "  return &G1;"
  , "}"
  , "/* test_macrosreparsenestingunion_i_Example_get_G2 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_2290f6729b2485e1 (void)"
  , "{"
  , "  return &G2;"
  , "}"
  , "/* test_macrosreparsenestingunion_i_Example_get_G3 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_1be42b84691d63f3 (void)"
  , "{"
  , "  return &G3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G1@
foreign import ccall unsafe "hs_bindgen_4b01ee4d60407c60" hs_bindgen_4b01ee4d60407c60_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G1@
hs_bindgen_4b01ee4d60407c60 :: IO (RIP.Ptr G1)
hs_bindgen_4b01ee4d60407c60 =
  RIP.fromFFIType hs_bindgen_4b01ee4d60407c60_base

{-# NOINLINE g1 #-}
{-| __C declaration:__ @G1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 3:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
g1 :: RIP.Ptr G1
g1 = RIP.unsafePerformIO hs_bindgen_4b01ee4d60407c60

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G2@
foreign import ccall unsafe "hs_bindgen_2290f6729b2485e1" hs_bindgen_2290f6729b2485e1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G2@
hs_bindgen_2290f6729b2485e1 :: IO (RIP.Ptr (RIP.Ptr G2))
hs_bindgen_2290f6729b2485e1 =
  RIP.fromFFIType hs_bindgen_2290f6729b2485e1_base

{-# NOINLINE g2 #-}
{-| __C declaration:__ @G2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 4:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
g2 :: RIP.Ptr (RIP.Ptr G2)
g2 = RIP.unsafePerformIO hs_bindgen_2290f6729b2485e1

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G3@
foreign import ccall unsafe "hs_bindgen_1be42b84691d63f3" hs_bindgen_1be42b84691d63f3_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_G3@
hs_bindgen_1be42b84691d63f3 :: IO (RIP.Ptr (RIP.Ptr (RIP.Ptr G3)))
hs_bindgen_1be42b84691d63f3 =
  RIP.fromFFIType hs_bindgen_1be42b84691d63f3_base

{-# NOINLINE g3 #-}
{-| __C declaration:__ @G3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_variable.h 5:23@

    __exported by:__ @macros\/reparse\/nesting\/union_in_variable.h@
-}
g3 :: RIP.Ptr (RIP.Ptr (RIP.Ptr G3))
g3 = RIP.unsafePerformIO hs_bindgen_1be42b84691d63f3
