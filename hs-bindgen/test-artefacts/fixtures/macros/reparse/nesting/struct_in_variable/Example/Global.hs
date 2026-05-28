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
  [ "#include <macros/reparse/nesting/struct_in_variable.h>"
  , "/* test_macrosreparsenestingstruct__Example_get_G1 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_7b488cf23d974403 (void)"
  , "{"
  , "  return &G1;"
  , "}"
  , "/* test_macrosreparsenestingstruct__Example_get_G2 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_be835215a4ac0819 (void)"
  , "{"
  , "  return &G2;"
  , "}"
  , "/* test_macrosreparsenestingstruct__Example_get_G3 */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_dad785bf89d140bb (void)"
  , "{"
  , "  return &G3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G1@
foreign import ccall unsafe "hs_bindgen_7b488cf23d974403" hs_bindgen_7b488cf23d974403_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G1@
hs_bindgen_7b488cf23d974403 :: IO (RIP.Ptr G1)
hs_bindgen_7b488cf23d974403 =
  RIP.fromFFIType hs_bindgen_7b488cf23d974403_base

{-# NOINLINE g1 #-}
{-| __C declaration:__ @G1@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_variable.h 3:24@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_variable.h@
-}
g1 :: RIP.Ptr G1
g1 = RIP.unsafePerformIO hs_bindgen_7b488cf23d974403

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G2@
foreign import ccall unsafe "hs_bindgen_be835215a4ac0819" hs_bindgen_be835215a4ac0819_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G2@
hs_bindgen_be835215a4ac0819 :: IO (RIP.Ptr (RIP.Ptr G2))
hs_bindgen_be835215a4ac0819 =
  RIP.fromFFIType hs_bindgen_be835215a4ac0819_base

{-# NOINLINE g2 #-}
{-| __C declaration:__ @G2@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_variable.h 4:24@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_variable.h@
-}
g2 :: RIP.Ptr (RIP.Ptr G2)
g2 = RIP.unsafePerformIO hs_bindgen_be835215a4ac0819

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G3@
foreign import ccall unsafe "hs_bindgen_dad785bf89d140bb" hs_bindgen_dad785bf89d140bb_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparsenestingstruct__Example_get_G3@
hs_bindgen_dad785bf89d140bb :: IO (RIP.Ptr (RIP.Ptr (RIP.Ptr G3)))
hs_bindgen_dad785bf89d140bb =
  RIP.fromFFIType hs_bindgen_dad785bf89d140bb_base

{-# NOINLINE g3 #-}
{-| __C declaration:__ @G3@

    __defined at:__ @macros\/reparse\/nesting\/struct_in_variable.h 5:24@

    __exported by:__ @macros\/reparse\/nesting\/struct_in_variable.h@
-}
g3 :: RIP.Ptr (RIP.Ptr (RIP.Ptr G3))
g3 = RIP.unsafePerformIO hs_bindgen_dad785bf89d140bb
