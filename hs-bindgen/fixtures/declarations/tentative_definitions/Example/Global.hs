{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i1@
hs_bindgen_3a9fc2bb34e15eb6 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_3a9fc2bb34e15eb6 =
  RIP.fromFFIType hs_bindgen_3a9fc2bb34e15eb6_base

{-# NOINLINE i1 #-}
{-| __C declaration:__ @i1@

    __defined at:__ @declarations\/tentative_definitions.h 17:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i1 :: RIP.Ptr RIP.CInt
i1 = RIP.unsafePerformIO hs_bindgen_3a9fc2bb34e15eb6

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_831c291120790ea6" hs_bindgen_831c291120790ea6_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
hs_bindgen_831c291120790ea6 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_831c291120790ea6 =
  RIP.fromFFIType hs_bindgen_831c291120790ea6_base

{-# NOINLINE i2 #-}
{-| __C declaration:__ @i2@

    __defined at:__ @declarations\/tentative_definitions.h 21:12@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i2 :: RIP.Ptr RIP.CInt
i2 = RIP.unsafePerformIO hs_bindgen_831c291120790ea6

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_6c7d3ba4369a4d65" hs_bindgen_6c7d3ba4369a4d65_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
hs_bindgen_6c7d3ba4369a4d65 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_6c7d3ba4369a4d65 =
  RIP.fromFFIType hs_bindgen_6c7d3ba4369a4d65_base

{-# NOINLINE i3 #-}
{-| __C declaration:__ @i3@

    __defined at:__ @declarations\/tentative_definitions.h 30:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i3 :: RIP.Ptr RIP.CInt
i3 = RIP.unsafePerformIO hs_bindgen_6c7d3ba4369a4d65
