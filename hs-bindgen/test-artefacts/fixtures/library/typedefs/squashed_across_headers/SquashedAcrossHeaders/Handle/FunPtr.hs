{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module SquashedAcrossHeaders.Handle.FunPtr
    ( SquashedAcrossHeaders.Handle.FunPtr.handle_new
    , SquashedAcrossHeaders.Handle.FunPtr.handle_use
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified SquashedAcrossHeaders.A
import qualified SquashedAcrossHeaders.Private.Handle
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <squashed_across_headers.h>"
  , "/* test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_new */"
  , "__attribute__ ((const))"
  , "handle *(*hs_bindgen_1c99e24011a56183 (void)) ("
  , "  a *arg1"
  , ")"
  , "{"
  , "  return &handle_new;"
  , "}"
  , "/* test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_use */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_89d0743825bb25cc (void)) ("
  , "  handle *arg1"
  , ")"
  , "{"
  , "  return &handle_use;"
  , "}"
  ]))

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_new@
foreign import ccall unsafe "hs_bindgen_1c99e24011a56183" hs_bindgen_1c99e24011a56183_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_new@
hs_bindgen_1c99e24011a56183 :: IO (BG.FunPtr (BG.Ptr SquashedAcrossHeaders.A.A -> IO (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle)))
hs_bindgen_1c99e24011a56183 =
  fmap BG.fromFFIType hs_bindgen_1c99e24011a56183_base

{-# NOINLINE handle_new #-}
{-| __C declaration:__ @handle_new@

    __defined at:__ @..\/handle.h 11:9@

    __exported by:__ @squashed_across_headers.h@
-}
handle_new :: BG.FunPtr (BG.Ptr SquashedAcrossHeaders.A.A -> IO (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle))
handle_new =
  BG.unsafePerformIO hs_bindgen_1c99e24011a56183

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_use@
foreign import ccall unsafe "hs_bindgen_89d0743825bb25cc" hs_bindgen_89d0743825bb25cc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarytypedefssquashed_acro_SquashedAcrossHeaders.Handle_get_handle_use@
hs_bindgen_89d0743825bb25cc :: IO (BG.FunPtr (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle -> IO BG.CInt))
hs_bindgen_89d0743825bb25cc =
  fmap BG.fromFFIType hs_bindgen_89d0743825bb25cc_base

{-# NOINLINE handle_use #-}
{-| __C declaration:__ @handle_use@

    __defined at:__ @..\/handle.h 12:5@

    __exported by:__ @squashed_across_headers.h@
-}
handle_use :: BG.FunPtr (BG.Ptr SquashedAcrossHeaders.Private.Handle.Handle -> IO BG.CInt)
handle_use =
  BG.unsafePerformIO hs_bindgen_89d0743825bb25cc
