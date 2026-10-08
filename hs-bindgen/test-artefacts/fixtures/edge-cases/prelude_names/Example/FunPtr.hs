{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fmap'
    , Example.FunPtr.reverse
    , Example.FunPtr.identity
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/prelude_names.h>"
  , "/* test_edgecasesprelude_names_Example_get_fmap */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8ec725a8c9df6bc9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fmap;"
  , "}"
  , "/* test_edgecasesprelude_names_Example_get_reverse */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a7d70ec0c8acc7e7 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &reverse;"
  , "}"
  , "/* test_edgecasesprelude_names_Example_get_identity */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_535374ba193530ae (void)) ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return &identity;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesprelude_names_Example_get_fmap@
foreign import ccall unsafe "hs_bindgen_8ec725a8c9df6bc9" hs_bindgen_8ec725a8c9df6bc9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_get_fmap@
hs_bindgen_8ec725a8c9df6bc9 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_8ec725a8c9df6bc9 =
  fmap BG.fromFFIType hs_bindgen_8ec725a8c9df6bc9_base

{-# NOINLINE fmap' #-}
{-| __C declaration:__ @fmap@

    __defined at:__ @edge-cases\/prelude_names.h 5:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
fmap' :: BG.FunPtr (BG.CInt -> IO BG.CInt)
fmap' =
  BG.unsafePerformIO hs_bindgen_8ec725a8c9df6bc9

-- __unique:__ @test_edgecasesprelude_names_Example_get_reverse@
foreign import ccall unsafe "hs_bindgen_a7d70ec0c8acc7e7" hs_bindgen_a7d70ec0c8acc7e7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_get_reverse@
hs_bindgen_a7d70ec0c8acc7e7 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_a7d70ec0c8acc7e7 =
  fmap BG.fromFFIType hs_bindgen_a7d70ec0c8acc7e7_base

{-# NOINLINE reverse #-}
{-| __C declaration:__ @reverse@

    __defined at:__ @edge-cases\/prelude_names.h 18:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
reverse :: BG.FunPtr (BG.CInt -> IO BG.CInt)
reverse =
  BG.unsafePerformIO hs_bindgen_a7d70ec0c8acc7e7

-- __unique:__ @test_edgecasesprelude_names_Example_get_identity@
foreign import ccall unsafe "hs_bindgen_535374ba193530ae" hs_bindgen_535374ba193530ae_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_get_identity@
hs_bindgen_535374ba193530ae :: IO (BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.Void)))
hs_bindgen_535374ba193530ae =
  fmap BG.fromFFIType hs_bindgen_535374ba193530ae_base

{-# NOINLINE identity #-}
{-| __C declaration:__ @identity@

    __defined at:__ @edge-cases\/prelude_names.h 31:7@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
identity :: BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.Void))
identity =
  BG.unsafePerformIO hs_bindgen_535374ba193530ae
