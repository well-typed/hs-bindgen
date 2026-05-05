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
  [ "#include <macros/redeclaration/different.h>"
  , "void hs_bindgen_07088bb3b4171c29 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_190787b2a527df76 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdifferent_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_07088bb3b4171c29" hs_bindgen_07088bb3b4171c29_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Safe_foo@
hs_bindgen_07088bb3b4171c29 ::
     RIP.CInt
  -> IO ()
hs_bindgen_07088bb3b4171c29 =
  RIP.fromFFIType hs_bindgen_07088bb3b4171c29_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/different.h 4:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_07088bb3b4171c29

-- __unique:__ @test_macrosredeclarationdifferent_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_190787b2a527df76" hs_bindgen_190787b2a527df76_base ::
     RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Safe_bar@
hs_bindgen_190787b2a527df76 ::
     RIP.CChar
  -> IO ()
hs_bindgen_190787b2a527df76 =
  RIP.fromFFIType hs_bindgen_190787b2a527df76_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/different.h 6:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
bar ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_190787b2a527df76
