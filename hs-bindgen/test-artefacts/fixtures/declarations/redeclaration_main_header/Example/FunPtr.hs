{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/redeclaration_main_header.h>"
  , "/* test_declarationsredeclaration_mai_Example_get_f */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ee8e2a043c1d908e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_declarationsredeclaration_mai_Example_get_f@
foreign import ccall unsafe "hs_bindgen_ee8e2a043c1d908e" hs_bindgen_ee8e2a043c1d908e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_declarationsredeclaration_mai_Example_get_f@
hs_bindgen_ee8e2a043c1d908e :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_ee8e2a043c1d908e =
  fmap BG.fromFFIType hs_bindgen_ee8e2a043c1d908e_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @declarations\/redeclaration_main_header.h 5:5@

    __exported by:__ @declarations\/redeclaration_main_header.h@
-}
f :: BG.FunPtr (BG.CInt -> IO BG.CInt)
f = BG.unsafePerformIO hs_bindgen_ee8e2a043c1d908e
