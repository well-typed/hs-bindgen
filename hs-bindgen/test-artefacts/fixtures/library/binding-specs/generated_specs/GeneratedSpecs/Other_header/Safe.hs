{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module GeneratedSpecs.Other_header.Safe
    ( GeneratedSpecs.Other_header.Safe.other_header_make
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import GeneratedSpecs.Other_header
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <generated_specs.h>"
  , "void hs_bindgen_ae90947491835d95 ("
  , "  double arg1,"
  , "  other_header *arg2"
  , ")"
  , "{"
  , "  *arg2 = (other_header_make)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Other_header_Safe_other_header_make@
foreign import ccall safe "hs_bindgen_ae90947491835d95" hs_bindgen_ae90947491835d95_base ::
     BG.CDouble
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Other_header_Safe_other_header_make@
hs_bindgen_ae90947491835d95 ::
     BG.CDouble
  -> BG.Ptr Other_header
  -> IO ()
hs_bindgen_ae90947491835d95 =
  \x0 ->
    \x1 ->
      hs_bindgen_ae90947491835d95_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @other_header_make@

    __defined at:__ @generated_specs\/other_header.h 10:14@

    __exported by:__ @generated_specs.h@
-}
other_header_make ::
     BG.CDouble
     -- ^ __C declaration:__ @value@
  -> IO Other_header
other_header_make =
  \value0 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_ae90947491835d95 value0 res1)
