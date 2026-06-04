{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.t1
    , Example.Global.t2
    , Example.Global.t3
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/nesting/union_in_struct.h>"
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T1@
hs_bindgen_44a1ef54c1044601 :: IO (RIP.Ptr T1)
hs_bindgen_44a1ef54c1044601 =
  RIP.fromFFIType hs_bindgen_44a1ef54c1044601_base

{-# NOINLINE t1 #-}
{-| __C declaration:__ @T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:37@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
t1 :: RIP.Ptr T1
t1 = RIP.unsafePerformIO hs_bindgen_44a1ef54c1044601

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T2@
foreign import ccall unsafe "hs_bindgen_915293812630a04d" hs_bindgen_915293812630a04d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T2@
hs_bindgen_915293812630a04d :: IO (RIP.Ptr T2)
hs_bindgen_915293812630a04d =
  RIP.fromFFIType hs_bindgen_915293812630a04d_base

{-# NOINLINE t2 #-}
{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:37@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
t2 :: RIP.Ptr T2
t2 = RIP.unsafePerformIO hs_bindgen_915293812630a04d

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T3@
foreign import ccall unsafe "hs_bindgen_e20a6c1bc9b97a9e" hs_bindgen_e20a6c1bc9b97a9e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingunion_i_Example_get_T3@
hs_bindgen_e20a6c1bc9b97a9e :: IO (RIP.Ptr T3)
hs_bindgen_e20a6c1bc9b97a9e =
  RIP.fromFFIType hs_bindgen_e20a6c1bc9b97a9e_base

{-# NOINLINE t3 #-}
{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:37@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
t3 :: RIP.Ptr T3
t3 = RIP.unsafePerformIO hs_bindgen_e20a6c1bc9b97a9e
