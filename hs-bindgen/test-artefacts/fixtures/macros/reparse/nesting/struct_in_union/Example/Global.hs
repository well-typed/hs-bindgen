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
  [ "#include <macros/reparse/nesting/struct_in_union.h>"
  , "/* test_macrosreparsenestingstruct__Example_get_T1 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_2ac02abef90be65b (void)"
  , "{"
  , "  return &T1;"
  , "}"
  , "/* test_macrosreparsenestingstruct__Example_get_T2 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_ce4ed8cb010301fc (void)"
  , "{"
  , "  return &T2;"
  , "}"
  , "/* test_macrosreparsenestingstruct__Example_get_T3 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_9d78eb12f80bd5a1 (void)"
  , "{"
  , "  return &T3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T1@
foreign import ccall unsafe "hs_bindgen_2ac02abef90be65b" hs_bindgen_2ac02abef90be65b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T1@
hs_bindgen_2ac02abef90be65b :: IO (BG.Ptr T1)
hs_bindgen_2ac02abef90be65b =
  BG.fromFFIType hs_bindgen_2ac02abef90be65b_base

{-# NOINLINE t1 #-}
{-| __C declaration:__ @T1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 3:37@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
t1 :: BG.Ptr T1
t1 = BG.unsafePerformIO hs_bindgen_2ac02abef90be65b

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T2@
foreign import ccall unsafe "hs_bindgen_ce4ed8cb010301fc" hs_bindgen_ce4ed8cb010301fc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T2@
hs_bindgen_ce4ed8cb010301fc :: IO (BG.Ptr T2)
hs_bindgen_ce4ed8cb010301fc =
  BG.fromFFIType hs_bindgen_ce4ed8cb010301fc_base

{-# NOINLINE t2 #-}
{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 4:37@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
t2 :: BG.Ptr T2
t2 = BG.unsafePerformIO hs_bindgen_ce4ed8cb010301fc

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T3@
foreign import ccall unsafe "hs_bindgen_9d78eb12f80bd5a1" hs_bindgen_9d78eb12f80bd5a1_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T3@
hs_bindgen_9d78eb12f80bd5a1 :: IO (BG.Ptr T3)
hs_bindgen_9d78eb12f80bd5a1 =
  BG.fromFFIType hs_bindgen_9d78eb12f80bd5a1_base

{-# NOINLINE t3 #-}
{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_union.h 5:37@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_union.h@
-}
t3 :: BG.Ptr T3
t3 = BG.unsafePerformIO hs_bindgen_9d78eb12f80bd5a1
