{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/no_ffitype.h>"
  , "void hs_bindgen_51195acecf6b880e ("
  , "  C arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "C hs_bindgen_44994e62548d5196 (void)"
  , "{"
  , "  return (bar)();"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_51195acecf6b880e" hs_bindgen_51195acecf6b880e_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_foo@
hs_bindgen_51195acecf6b880e ::
     M.C
  -> IO ()
hs_bindgen_51195acecf6b880e =
  \x0 ->
    hs_bindgen_51195acecf6b880e_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
foo ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_51195acecf6b880e

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_44994e62548d5196" hs_bindgen_44994e62548d5196_base ::
     IO BG.CInt

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Unsafe_bar@
hs_bindgen_44994e62548d5196 :: IO M.C
hs_bindgen_44994e62548d5196 =
  fmap BG.fromFFIType hs_bindgen_44994e62548d5196_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 7:3@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
bar :: IO M.C
bar = hs_bindgen_44994e62548d5196
