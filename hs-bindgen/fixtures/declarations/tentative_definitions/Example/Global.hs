{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i1@
hs_bindgen_3a9fc2bb34e15eb6 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_3a9fc2bb34e15eb6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3a9fc2bb34e15eb6_base

{-# NOINLINE i1 #-}
{-| __C declaration:__ @i1@

    __defined at:__ @declarations\/tentative_definitions.h 17:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i1 :: Ptr.Ptr FC.CInt
i1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3a9fc2bb34e15eb6

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_831c291120790ea6" hs_bindgen_831c291120790ea6_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i2@
hs_bindgen_831c291120790ea6 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_831c291120790ea6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_831c291120790ea6_base

{-# NOINLINE i2 #-}
{-| __C declaration:__ @i2@

    __defined at:__ @declarations\/tentative_definitions.h 21:12@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i2 :: Ptr.Ptr FC.CInt
i2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_831c291120790ea6

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_6c7d3ba4369a4d65" hs_bindgen_6c7d3ba4369a4d65_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_declarationstentative_definit_Example_get_i3@
hs_bindgen_6c7d3ba4369a4d65 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_6c7d3ba4369a4d65 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6c7d3ba4369a4d65_base

{-# NOINLINE i3 #-}
{-| __C declaration:__ @i3@

    __defined at:__ @declarations\/tentative_definitions.h 30:5@

    __exported by:__ @declarations\/tentative_definitions.h@
-}
i3 :: Ptr.Ptr FC.CInt
i3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6c7d3ba4369a4d65
