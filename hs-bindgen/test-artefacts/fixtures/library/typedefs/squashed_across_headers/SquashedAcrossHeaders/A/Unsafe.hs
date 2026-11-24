{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module SquashedAcrossHeaders.A.Unsafe
    ( SquashedAcrossHeaders.A.Unsafe.a_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import SquashedAcrossHeaders.A

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <squashed_across_headers.h>"
  , "a *hs_bindgen_0d2520bbfd9a9e4e (void)"
  , "{"
  , "  return (a_new)();"
  , "}"
  ]))

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.A_Unsafe_a_new@
foreign import ccall unsafe "hs_bindgen_0d2520bbfd9a9e4e" hs_bindgen_0d2520bbfd9a9e4e_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.A_Unsafe_a_new@
hs_bindgen_0d2520bbfd9a9e4e :: IO (BG.Ptr A)
hs_bindgen_0d2520bbfd9a9e4e =
  fmap BG.fromFFIType hs_bindgen_0d2520bbfd9a9e4e_base

{-| __C declaration:__ @a_new@

    __defined at:__ @squashed_across_headers\/a.h 13:4@

    __exported by:__ @squashed_across_headers.h@
-}
a_new :: IO (BG.Ptr A)
a_new = hs_bindgen_0d2520bbfd9a9e4e
