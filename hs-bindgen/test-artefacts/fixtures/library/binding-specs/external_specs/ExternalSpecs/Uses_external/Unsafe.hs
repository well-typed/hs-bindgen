{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ExternalSpecs.Uses_external.Unsafe
    ( ExternalSpecs.Uses_external.Unsafe.uses_external_make
    )
  where

import qualified GeneratedSpecs.Other_header
import qualified GeneratedSpecs.Same_header
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import ExternalSpecs.Uses_external
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <external_specs.h>"
  , "void hs_bindgen_95305bf47186e5c9 ("
  , "  same_header *arg1,"
  , "  other_header *arg2,"
  , "  struct uses_external *arg3"
  , ")"
  , "{"
  , "  *arg3 = (uses_external_make)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsexternal_ExternalSpecs.Uses_external_Unsafe_uses_external_make@
foreign import ccall unsafe "hs_bindgen_95305bf47186e5c9" hs_bindgen_95305bf47186e5c9_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarybindingspecsexternal_ExternalSpecs.Uses_external_Unsafe_uses_external_make@
hs_bindgen_95305bf47186e5c9 ::
     BG.Ptr GeneratedSpecs.Same_header.Same_header
  -> BG.Ptr GeneratedSpecs.Other_header.Other_header
  -> BG.Ptr Uses_external
  -> IO ()
hs_bindgen_95305bf47186e5c9 =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_95305bf47186e5c9_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

{-| __C declaration:__ @uses_external_make@

    __defined at:__ @external_specs\/uses_external.h 15:22@

    __exported by:__ @external_specs.h@
-}
uses_external_make ::
     GeneratedSpecs.Same_header.Same_header
     -- ^ __C declaration:__ @a@
  -> GeneratedSpecs.Other_header.Other_header
     -- ^ __C declaration:__ @b@
  -> IO Uses_external
uses_external_make =
  \a0 ->
    \b1 ->
      BG.with a0 (\a2 ->
                    BG.with b1 (\b3 ->
                                  BG.allocaAndPeek (\res4 ->
                                                      hs_bindgen_95305bf47186e5c9 a2 b3 res4)))
