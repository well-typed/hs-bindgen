{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.fmap'
    , Example.Unsafe.reverse
    , Example.Unsafe.identity
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/prelude_names.h>"
  , "signed int hs_bindgen_b6805fb9ba57fb59 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (fmap)(arg1);"
  , "}"
  , "signed int hs_bindgen_e4643018a1638d28 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (reverse)(arg1);"
  , "}"
  , "void *hs_bindgen_3cdf92558d7dc09d ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return (identity)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_fmap@
foreign import ccall unsafe "hs_bindgen_b6805fb9ba57fb59" hs_bindgen_b6805fb9ba57fb59_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_fmap@
hs_bindgen_b6805fb9ba57fb59 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_b6805fb9ba57fb59 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b6805fb9ba57fb59_base (BG.toFFIType x0))

{-| __C declaration:__ @fmap@

    __defined at:__ @edge-cases\/prelude_names.h 5:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
fmap' ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
fmap' = hs_bindgen_b6805fb9ba57fb59

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_reverse@
foreign import ccall unsafe "hs_bindgen_e4643018a1638d28" hs_bindgen_e4643018a1638d28_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_reverse@
hs_bindgen_e4643018a1638d28 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_e4643018a1638d28 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_e4643018a1638d28_base (BG.toFFIType x0))

{-| __C declaration:__ @reverse@

    __defined at:__ @edge-cases\/prelude_names.h 18:5@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
reverse ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
reverse = hs_bindgen_e4643018a1638d28

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_identity@
foreign import ccall unsafe "hs_bindgen_3cdf92558d7dc09d" hs_bindgen_3cdf92558d7dc09d_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_Unsafe_identity@
hs_bindgen_3cdf92558d7dc09d ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)
hs_bindgen_3cdf92558d7dc09d =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_3cdf92558d7dc09d_base (BG.toFFIType x0))

{-| __C declaration:__ @identity@

    __defined at:__ @edge-cases\/prelude_names.h 31:7@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
identity ::
     BG.Ptr BG.Void
     -- ^ __C declaration:__ @ptr@
  -> IO (BG.Ptr BG.Void)
identity = hs_bindgen_3cdf92558d7dc09d
