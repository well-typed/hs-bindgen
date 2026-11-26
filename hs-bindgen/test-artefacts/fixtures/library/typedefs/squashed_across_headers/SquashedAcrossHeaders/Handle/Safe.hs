{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module SquashedAcrossHeaders.Handle.Safe
    ( SquashedAcrossHeaders.Handle.Safe.handle_new
    , SquashedAcrossHeaders.Handle.Safe.handle_use
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified SquashedAcrossHeaders.A
import qualified SquashedAcrossHeaders.Private.Handle
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <squashed_across_headers.h>"
  , "handle *hs_bindgen_1e3c2f4042f9525a ("
  , "  a *arg1"
  , ")"
  , "{"
  , "  return (handle_new)(arg1);"
  , "}"
  , "signed int hs_bindgen_2bb57e67beee35cf ("
  , "  handle *arg1"
  , ")"
  , "{"
  , "  return (handle_use)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_Safe_handle_new@
foreign import ccall safe "hs_bindgen_1e3c2f4042f9525a" hs_bindgen_1e3c2f4042f9525a_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_Safe_handle_new@
hs_bindgen_1e3c2f4042f9525a ::
     BG.Ptr SquashedAcrossHeaders.A.A
  -> IO (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle)
hs_bindgen_1e3c2f4042f9525a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1e3c2f4042f9525a_base (BG.toFFIType x0))

{-| __C declaration:__ @handle_new@

    __defined at:__ @..\/handle.h 11:9@

    __exported by:__ @squashed_across_headers.h@
-}
handle_new ::
     BG.Ptr SquashedAcrossHeaders.A.A
     -- ^ __C declaration:__ @owner@
  -> IO (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle)
handle_new = hs_bindgen_1e3c2f4042f9525a

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_Safe_handle_use@
foreign import ccall safe "hs_bindgen_2bb57e67beee35cf" hs_bindgen_2bb57e67beee35cf_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_Safe_handle_use@
hs_bindgen_2bb57e67beee35cf ::
     BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle
  -> IO BG.CInt
hs_bindgen_2bb57e67beee35cf =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2bb57e67beee35cf_base (BG.toFFIType x0))

{-| __C declaration:__ @handle_use@

    __defined at:__ @..\/handle.h 12:5@

    __exported by:__ @squashed_across_headers.h@
-}
handle_use ::
     BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle
     -- ^ __C declaration:__ @h@
  -> IO BG.CInt
handle_use = hs_bindgen_2bb57e67beee35cf
