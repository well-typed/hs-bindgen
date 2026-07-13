{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.t1
    , Example.Global.t2
    , Example.Global.t3
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/nesting/union_in_union.h>"
  , "/* test_macrosreparsenestingunion_i_Example_get_T1 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_44a1ef54c1044601 (void)"
  , "{"
  , "  return &T1;"
  , "}"
  , "/* test_macrosreparsenestingunion_i_Example_get_T2 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_915293812630a04d (void)"
  , "{"
  , "  return &T2;"
  , "}"
  , "/* test_macrosreparsenestingunion_i_Example_get_T3 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_e20a6c1bc9b97a9e (void)"
  , "{"
  , "  return &T3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T1@
foreign import ccall unsafe "hs_bindgen_44a1ef54c1044601" hs_bindgen_44a1ef54c1044601_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T1@
hs_bindgen_44a1ef54c1044601 :: IO (BG.Ptr T1)
hs_bindgen_44a1ef54c1044601 =
  BG.fromFFIType hs_bindgen_44a1ef54c1044601_base

{-# NOINLINE t1 #-}
{-| __C declaration:__ @T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 3:36@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
t1 :: BG.Ptr T1
t1 = BG.unsafePerformIO hs_bindgen_44a1ef54c1044601

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T2@
foreign import ccall unsafe "hs_bindgen_915293812630a04d" hs_bindgen_915293812630a04d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T2@
hs_bindgen_915293812630a04d :: IO (BG.Ptr T2)
hs_bindgen_915293812630a04d =
  BG.fromFFIType hs_bindgen_915293812630a04d_base

{-# NOINLINE t2 #-}
{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 4:36@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
t2 :: BG.Ptr T2
t2 = BG.unsafePerformIO hs_bindgen_915293812630a04d

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T3@
foreign import ccall unsafe "hs_bindgen_e20a6c1bc9b97a9e" hs_bindgen_e20a6c1bc9b97a9e_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T3@
hs_bindgen_e20a6c1bc9b97a9e :: IO (BG.Ptr T3)
hs_bindgen_e20a6c1bc9b97a9e =
  BG.fromFFIType hs_bindgen_e20a6c1bc9b97a9e_base

{-# NOINLINE t3 #-}
{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_union.h 5:36@

    __exported by:__ @macros\/reparse\/nesting\/union_in_union.h@
-}
t3 :: BG.Ptr T3
t3 = BG.unsafePerformIO hs_bindgen_e20a6c1bc9b97a9e
