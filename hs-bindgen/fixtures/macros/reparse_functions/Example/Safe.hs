{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse_functions.h>"
  , "void hs_bindgen_1ba8c1f55c7c7927 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_e7e5457598cc9f22 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_functions_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_1ba8c1f55c7c7927" hs_bindgen_1ba8c1f55c7c7927_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_functions_Example_Safe_foo@
hs_bindgen_1ba8c1f55c7c7927 ::
     RIP.CInt
  -> IO ()
hs_bindgen_1ba8c1f55c7c7927 =
  RIP.fromFFIType hs_bindgen_1ba8c1f55c7c7927_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse_functions.h 6:6@

    __exported by:__ @macros\/reparse_functions.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_1ba8c1f55c7c7927

-- __unique:__ @test_macrosreparse_functions_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_e7e5457598cc9f22" hs_bindgen_e7e5457598cc9f22_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_functions_Example_Safe_bar@
hs_bindgen_e7e5457598cc9f22 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_e7e5457598cc9f22 =
  RIP.fromFFIType hs_bindgen_e7e5457598cc9f22_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse_functions.h 11:19@

    __exported by:__ @macros\/reparse_functions.h@
-}
bar ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
bar = hs_bindgen_e7e5457598cc9f22
