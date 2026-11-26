{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module SquashedAcrossHeaders.A.FunPtr
    ( SquashedAcrossHeaders.A.FunPtr.a_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import SquashedAcrossHeaders.A

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <squashed_across_headers.h>"
  , "/* test_librarytypedefssquashed_acro_SquashedAcrossHeaders.A_get_a_new */"
  , "__attribute__ ((const))"
  , "a *(*hs_bindgen_cfd25547d0d1027a (void)) (void)"
  , "{"
  , "  return &a_new;"
  , "}"
  ]))

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.A_get_a_new@
foreign import ccall unsafe "hs_bindgen_cfd25547d0d1027a" hs_bindgen_cfd25547d0d1027a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.A_get_a_new@
hs_bindgen_cfd25547d0d1027a :: IO (BG.FunPtr (IO (BG.Ptr A)))
hs_bindgen_cfd25547d0d1027a =
  fmap BG.fromFFIType hs_bindgen_cfd25547d0d1027a_base

{-# NOINLINE a_new #-}
{-| __C declaration:__ @a_new@

    __defined at:__ @squashed_across_headers\/a.h 13:4@

    __exported by:__ @squashed_across_headers.h@
-}
a_new :: BG.FunPtr (IO (BG.Ptr A))
a_new =
  BG.unsafePerformIO hs_bindgen_cfd25547d0d1027a
