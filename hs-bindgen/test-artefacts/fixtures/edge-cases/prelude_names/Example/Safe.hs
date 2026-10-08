{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.fmap'
    , Example.Safe.reverse
    , Example.Safe.identity
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/prelude_names.h>"
  , "signed int hs_bindgen_06a0882e0fd9fc01 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (fmap)(arg1);"
  , "}"
  , "signed int hs_bindgen_cb71068e35beb37d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (reverse)(arg1);"
  , "}"
  , "void *hs_bindgen_0de5f98d74dd3b0b ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return (identity)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_fmap@
foreign import ccall safe "hs_bindgen_06a0882e0fd9fc01" hs_bindgen_06a0882e0fd9fc01_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_fmap@
hs_bindgen_06a0882e0fd9fc01 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_06a0882e0fd9fc01 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_06a0882e0fd9fc01_base (BG.toFFIType x0))

{-| __C declaration:__ @fmap@

    __defined at:__ @edge-cases\/prelude_names.h 5:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
fmap' ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
fmap' = hs_bindgen_06a0882e0fd9fc01

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_reverse@
foreign import ccall safe "hs_bindgen_cb71068e35beb37d" hs_bindgen_cb71068e35beb37d_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_reverse@
hs_bindgen_cb71068e35beb37d ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_cb71068e35beb37d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_cb71068e35beb37d_base (BG.toFFIType x0))

{-| __C declaration:__ @reverse@

    __defined at:__ @edge-cases\/prelude_names.h 18:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
reverse ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
reverse = hs_bindgen_cb71068e35beb37d

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_identity@
foreign import ccall safe "hs_bindgen_0de5f98d74dd3b0b" hs_bindgen_0de5f98d74dd3b0b_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_Safe_identity@
hs_bindgen_0de5f98d74dd3b0b ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)
hs_bindgen_0de5f98d74dd3b0b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_0de5f98d74dd3b0b_base (BG.toFFIType x0))

{-| __C declaration:__ @identity@

    __defined at:__ @edge-cases\/prelude_names.h 31:7@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
identity ::
     BG.Ptr BG.Void
     -- ^ __C declaration:__ @ptr@
  -> IO (BG.Ptr BG.Void)
identity = hs_bindgen_0de5f98d74dd3b0b
