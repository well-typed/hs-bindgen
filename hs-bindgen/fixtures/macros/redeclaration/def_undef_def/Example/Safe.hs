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
  [ "#include <macros/redeclaration/def_undef_def.h>"
  , "void hs_bindgen_79fe9113b0a5d6a5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_4327dd00f245ace9 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_79fe9113b0a5d6a5" hs_bindgen_79fe9113b0a5d6a5_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Safe_foo@
hs_bindgen_79fe9113b0a5d6a5 ::
     RIP.CInt
  -> IO ()
hs_bindgen_79fe9113b0a5d6a5 =
  RIP.fromFFIType hs_bindgen_79fe9113b0a5d6a5_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 4:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_79fe9113b0a5d6a5

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_4327dd00f245ace9" hs_bindgen_4327dd00f245ace9_base ::
     RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Safe_bar@
hs_bindgen_4327dd00f245ace9 ::
     RIP.CChar
  -> IO ()
hs_bindgen_4327dd00f245ace9 =
  RIP.fromFFIType hs_bindgen_4327dd00f245ace9_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 7:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
bar ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_4327dd00f245ace9
