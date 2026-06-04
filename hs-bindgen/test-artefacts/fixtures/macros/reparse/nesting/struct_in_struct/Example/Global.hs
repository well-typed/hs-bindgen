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
  [ "#include <macros/reparse/nesting/struct_in_struct.h>"
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T1@
hs_bindgen_2ac02abef90be65b :: IO (RIP.Ptr T1)
hs_bindgen_2ac02abef90be65b =
  RIP.fromFFIType hs_bindgen_2ac02abef90be65b_base

{-# NOINLINE t1 #-}
{-| __C declaration:__ @T1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 3:38@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
t1 :: RIP.Ptr T1
t1 = RIP.unsafePerformIO hs_bindgen_2ac02abef90be65b

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T2@
foreign import ccall unsafe "hs_bindgen_ce4ed8cb010301fc" hs_bindgen_ce4ed8cb010301fc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T2@
hs_bindgen_ce4ed8cb010301fc :: IO (RIP.Ptr T2)
hs_bindgen_ce4ed8cb010301fc =
  RIP.fromFFIType hs_bindgen_ce4ed8cb010301fc_base

{-# NOINLINE t2 #-}
{-| __C declaration:__ @T2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 4:38@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
t2 :: RIP.Ptr T2
t2 = RIP.unsafePerformIO hs_bindgen_ce4ed8cb010301fc

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T3@
foreign import ccall unsafe "hs_bindgen_9d78eb12f80bd5a1" hs_bindgen_9d78eb12f80bd5a1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_T3@
hs_bindgen_9d78eb12f80bd5a1 :: IO (RIP.Ptr T3)
hs_bindgen_9d78eb12f80bd5a1 =
  RIP.fromFFIType hs_bindgen_9d78eb12f80bd5a1_base

{-# NOINLINE t3 #-}
{-| __C declaration:__ @T3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_struct.h 5:38@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_struct.h@
-}
t3 :: RIP.Ptr T3
t3 = RIP.unsafePerformIO hs_bindgen_9d78eb12f80bd5a1
