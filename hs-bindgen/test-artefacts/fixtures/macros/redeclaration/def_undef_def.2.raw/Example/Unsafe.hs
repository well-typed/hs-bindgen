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
  [ "#include <macros/redeclaration/def_undef_def.h>"
  , "void hs_bindgen_cd656f42f4fc25f4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_fe4fb459e811e272 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_cd656f42f4fc25f4" hs_bindgen_cd656f42f4fc25f4_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Unsafe_foo@
hs_bindgen_cd656f42f4fc25f4 ::
     BG.CInt
  -> IO ()
hs_bindgen_cd656f42f4fc25f4 =
  BG.fromFFIType hs_bindgen_cd656f42f4fc25f4_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 4:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_cd656f42f4fc25f4

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_fe4fb459e811e272" hs_bindgen_fe4fb459e811e272_base ::
     BG.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationdef_undef_Example_Unsafe_bar@
hs_bindgen_fe4fb459e811e272 ::
     BG.CChar
  -> IO ()
hs_bindgen_fe4fb459e811e272 =
  BG.fromFFIType hs_bindgen_fe4fb459e811e272_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 7:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
bar ::
     BG.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_fe4fb459e811e272
