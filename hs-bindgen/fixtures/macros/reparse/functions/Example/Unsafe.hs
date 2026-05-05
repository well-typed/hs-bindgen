{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
  ]))

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_39eae3eb6460ea6a" hs_bindgen_39eae3eb6460ea6a_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_foo@
hs_bindgen_39eae3eb6460ea6a ::
     RIP.CInt
  -> IO ()
hs_bindgen_39eae3eb6460ea6a =
  RIP.fromFFIType hs_bindgen_39eae3eb6460ea6a_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 6:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_39eae3eb6460ea6a

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_eb2d164aacb2e06d" hs_bindgen_eb2d164aacb2e06d_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsefunctions_Example_Unsafe_bar@
hs_bindgen_eb2d164aacb2e06d ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_eb2d164aacb2e06d =
  RIP.fromFFIType hs_bindgen_eb2d164aacb2e06d_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 11:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
bar = hs_bindgen_eb2d164aacb2e06d
