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
  [ "#include <macros/reparse_functions.h>"
  , "void hs_bindgen_a055341479c059fc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_a16c7393534fd155 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_functions_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_a055341479c059fc" hs_bindgen_a055341479c059fc_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparse_functions_Example_Unsafe_foo@
hs_bindgen_a055341479c059fc ::
     RIP.CInt
  -> IO ()
hs_bindgen_a055341479c059fc =
  RIP.fromFFIType hs_bindgen_a055341479c059fc_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse_functions.h 6:6@

    __exported by:__ @macros\/reparse_functions.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_a055341479c059fc

-- __unique:__ @test_macrosreparse_functions_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_a16c7393534fd155" hs_bindgen_a16c7393534fd155_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_macrosreparse_functions_Example_Unsafe_bar@
hs_bindgen_a16c7393534fd155 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_a16c7393534fd155 =
  RIP.fromFFIType hs_bindgen_a16c7393534fd155_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse_functions.h 11:19@

    __exported by:__ @macros\/reparse_functions.h@
-}
bar ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
bar = hs_bindgen_a16c7393534fd155
