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
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_foo@
hs_bindgen_42c808e061719a11 ::
     RIP.CInt
  -> IO ()
hs_bindgen_42c808e061719a11 =
  RIP.fromFFIType hs_bindgen_42c808e061719a11_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/different.h 4:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_42c808e061719a11

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_06a03878580406ca" hs_bindgen_06a03878580406ca_base ::
     RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationdifferent_Example_Unsafe_bar@
hs_bindgen_06a03878580406ca ::
     RIP.CChar
  -> IO ()
hs_bindgen_06a03878580406ca =
  RIP.fromFFIType hs_bindgen_06a03878580406ca_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/different.h 6:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
bar ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_06a03878580406ca
