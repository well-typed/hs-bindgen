{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.i1
    , Example.Global.i2
    , Example.Global.i3
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/tentative_definitions.h>"
  , "/* test_declarationstentative_definit_Example_get_i1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_3a9fc2bb34e15eb6 (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_declarationstentative_definit_Example_get_i2 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_831c291120790ea6 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_declarationstentative_definit_Example_get_i3 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_6c7d3ba4369a4d65 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  ]))

-- __unique:__ @test_declarationstentative_definit_Example_get_i1@
foreign import ccall unsafe "hs_bindgen_3a9fc2bb34e15eb6" hs_bindgen_3a9fc2bb34e15eb6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i1@
hs_bindgen_3a9fc2bb34e15eb6 :: IO (BG.Ptr BG.CInt)
hs_bindgen_3a9fc2bb34e15eb6 =
  BG.fromFFIType hs_bindgen_3a9fc2bb34e15eb6_base

{-# NOINLINE i1 #-}
{-| __C declaration:__ @i1@

    __defined at:__ @declarations\/tentative_definitions.h 17:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i1 :: BG.Ptr BG.CInt
i1 = BG.unsafePerformIO hs_bindgen_3a9fc2bb34e15eb6

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_831c291120790ea6" hs_bindgen_831c291120790ea6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
hs_bindgen_831c291120790ea6 :: IO (BG.Ptr BG.CInt)
hs_bindgen_831c291120790ea6 =
  BG.fromFFIType hs_bindgen_831c291120790ea6_base

{-# NOINLINE i2 #-}
{-| __C declaration:__ @i2@

    __defined at:__ @declarations\/tentative_definitions.h 21:12@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i2 :: BG.Ptr BG.CInt
i2 = BG.unsafePerformIO hs_bindgen_831c291120790ea6

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_6c7d3ba4369a4d65" hs_bindgen_6c7d3ba4369a4d65_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
hs_bindgen_6c7d3ba4369a4d65 :: IO (BG.Ptr BG.CInt)
hs_bindgen_6c7d3ba4369a4d65 =
  BG.fromFFIType hs_bindgen_6c7d3ba4369a4d65_base

{-# NOINLINE i3 #-}
{-| __C declaration:__ @i3@

    __defined at:__ @declarations\/tentative_definitions.h 30:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i3 :: BG.Ptr BG.CInt
i3 = BG.unsafePerformIO hs_bindgen_6c7d3ba4369a4d65
