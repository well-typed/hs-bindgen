{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/redeclaration/different.h>"
  , "void hs_bindgen_42c808e061719a11 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_06a03878580406ca ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_42c808e061719a11" hs_bindgen_42c808e061719a11_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_foo@
hs_bindgen_42c808e061719a11 ::
     BG.CInt
  -> IO ()
hs_bindgen_42c808e061719a11 =
  BG.fromFFIType hs_bindgen_42c808e061719a11_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/different.h 3:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_42c808e061719a11

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_06a03878580406ca" hs_bindgen_06a03878580406ca_base ::
     BG.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_bar@
hs_bindgen_06a03878580406ca ::
     BG.CChar
  -> IO ()
hs_bindgen_06a03878580406ca =
  BG.fromFFIType hs_bindgen_06a03878580406ca_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/different.h 5:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
bar ::
     BG.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_06a03878580406ca
