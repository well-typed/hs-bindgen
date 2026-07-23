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
  , "void hs_bindgen_630c30830ae4c332 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_26fa4ddfc00fb318 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_c42f62cd14a1cefd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_630c30830ae4c332" hs_bindgen_630c30830ae4c332_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_foo@
hs_bindgen_630c30830ae4c332 ::
     BG.CInt
  -> IO ()
hs_bindgen_630c30830ae4c332 =
  BG.fromFFIType hs_bindgen_630c30830ae4c332_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_630c30830ae4c332

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_26fa4ddfc00fb318" hs_bindgen_26fa4ddfc00fb318_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_bar@
hs_bindgen_26fa4ddfc00fb318 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_26fa4ddfc00fb318 =
  BG.fromFFIType hs_bindgen_26fa4ddfc00fb318_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
bar = hs_bindgen_26fa4ddfc00fb318

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_c42f62cd14a1cefd" hs_bindgen_c42f62cd14a1cefd_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_Unsafe_baz@
hs_bindgen_c42f62cd14a1cefd ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_c42f62cd14a1cefd =
  BG.fromFFIType hs_bindgen_c42f62cd14a1cefd_base

{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
baz = hs_bindgen_c42f62cd14a1cefd
