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
  [ "#include <macros/redeclaration/identical_syntax.h>"
  , "void hs_bindgen_1f6a4a088740d7bc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_69080a62615bc0d8 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_1f6a4a088740d7bc" hs_bindgen_1f6a4a088740d7bc_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Unsafe_foo@
hs_bindgen_1f6a4a088740d7bc ::
     RIP.CInt
  -> IO ()
hs_bindgen_1f6a4a088740d7bc =
  RIP.fromFFIType hs_bindgen_1f6a4a088740d7bc_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_syntax.h 6:6@

    __exported by:__ @macros\/redeclaration\/identical_syntax.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_1f6a4a088740d7bc

-- __unique:__ @test_macrosredeclarationidentical_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_69080a62615bc0d8" hs_bindgen_69080a62615bc0d8_base ::
     RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Unsafe_bar@
hs_bindgen_69080a62615bc0d8 ::
     RIP.CChar
  -> IO ()
hs_bindgen_69080a62615bc0d8 =
  RIP.fromFFIType hs_bindgen_69080a62615bc0d8_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_syntax.h 10:6@

    __exported by:__ @macros\/redeclaration\/identical_syntax.h@
-}
bar ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_69080a62615bc0d8
