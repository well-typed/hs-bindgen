{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/no_ffitype.h>"
  , "/* test_bindingspecsfun_argtypedef_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2020929269c10652 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argtypedef_Example_get_bar */"
  , "__attribute__ ((const))"
  , "C (*hs_bindgen_d07fcdceb06f5a2c (void)) (void)"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_2020929269c10652" hs_bindgen_2020929269c10652_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_foo@
hs_bindgen_2020929269c10652 :: IO (BG.FunPtr (M.C -> IO ()))
hs_bindgen_2020929269c10652 =
  fmap BG.fromFFIType hs_bindgen_2020929269c10652_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
foo :: BG.FunPtr (M.C -> IO ())
foo = BG.unsafePerformIO hs_bindgen_2020929269c10652

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_d07fcdceb06f5a2c" hs_bindgen_d07fcdceb06f5a2c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_get_bar@
hs_bindgen_d07fcdceb06f5a2c :: IO (BG.FunPtr (IO M.C))
hs_bindgen_d07fcdceb06f5a2c =
  fmap BG.fromFFIType hs_bindgen_d07fcdceb06f5a2c_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h 7:3@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/no_ffitype.h@
-}
bar :: BG.FunPtr (IO M.C)
bar = BG.unsafePerformIO hs_bindgen_d07fcdceb06f5a2c
