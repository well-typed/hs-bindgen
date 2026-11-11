{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/no_ffitype.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "C hs_bindgen_b80066b7dbab19ae (void)"
  , "{"
  , "  return (bar)();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     M.C
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  \x0 ->
    hs_bindgen_99bb90e6d7637d2c_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
foo ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_99bb90e6d7637d2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_b80066b7dbab19ae" hs_bindgen_b80066b7dbab19ae_base ::
     IO BG.CInt

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
hs_bindgen_b80066b7dbab19ae :: IO M.C
hs_bindgen_b80066b7dbab19ae =
  fmap BG.fromFFIType hs_bindgen_b80066b7dbab19ae_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 7:3@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
bar :: IO M.C
bar = hs_bindgen_b80066b7dbab19ae
