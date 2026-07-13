{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    , Example.Unsafe.baz
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/functions.h>"
  , "void hs_bindgen_39eae3eb6460ea6a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_eb2d164aacb2e06d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_69a979a1e6a18eb1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_39eae3eb6460ea6a" hs_bindgen_39eae3eb6460ea6a_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_foo@
hs_bindgen_39eae3eb6460ea6a ::
     BG.CInt
  -> IO ()
hs_bindgen_39eae3eb6460ea6a =
  BG.fromFFIType hs_bindgen_39eae3eb6460ea6a_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_39eae3eb6460ea6a

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_eb2d164aacb2e06d" hs_bindgen_eb2d164aacb2e06d_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_bar@
hs_bindgen_eb2d164aacb2e06d ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_eb2d164aacb2e06d =
  BG.fromFFIType hs_bindgen_eb2d164aacb2e06d_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
bar = hs_bindgen_eb2d164aacb2e06d

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_69a979a1e6a18eb1" hs_bindgen_69a979a1e6a18eb1_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_baz@
hs_bindgen_69a979a1e6a18eb1 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_69a979a1e6a18eb1 =
  BG.fromFFIType hs_bindgen_69a979a1e6a18eb1_base

{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
baz = hs_bindgen_69a979a1e6a18eb1
