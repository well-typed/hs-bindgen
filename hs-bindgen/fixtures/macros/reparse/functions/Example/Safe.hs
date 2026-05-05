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
  [ "#include <macros/reparse/functions.h>"
  , "void hs_bindgen_b6e9ae739486d53b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_e44e9d05eacbc37e ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_b6e9ae739486d53b" hs_bindgen_b6e9ae739486d53b_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_Example_Safe_foo@
hs_bindgen_b6e9ae739486d53b ::
     RIP.CInt
  -> IO ()
hs_bindgen_b6e9ae739486d53b =
  RIP.fromFFIType hs_bindgen_b6e9ae739486d53b_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 6:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_b6e9ae739486d53b

-- __unique:__ @test_macrosreparsefunctions_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_e44e9d05eacbc37e" hs_bindgen_e44e9d05eacbc37e_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparsefunctions_Example_Safe_bar@
hs_bindgen_e44e9d05eacbc37e ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_e44e9d05eacbc37e =
  RIP.fromFFIType hs_bindgen_e44e9d05eacbc37e_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 11:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
bar = hs_bindgen_e44e9d05eacbc37e
