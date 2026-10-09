{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Api.Unsafe
    ( M.Api.Unsafe.api_run
    )
  where

import qualified HsBindgen.Runtime.Support
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Bar
import qualified M.Foo
import qualified M.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <forward_typedef/root.h>"
  , "lib_status hs_bindgen_8d90aaa10fcab83d ("
  , "  foo *arg1,"
  , "  bar *arg2"
  , ")"
  , "{"
  , "  return (api_run)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_typedef_M.Api_Unsafe_api_run@
foreign import ccall unsafe "hs_bindgen_8d90aaa10fcab83d" hs_bindgen_8d90aaa10fcab83d_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO HsBindgen.Runtime.Support.CInt

-- __unique:__ @test_libraryforward_typedef_M.Api_Unsafe_api_run@
hs_bindgen_8d90aaa10fcab83d ::
     BG.Ptr M.Foo.Foo
  -> BG.Ptr M.Bar.Bar
  -> IO M.Types.Lib_status
hs_bindgen_8d90aaa10fcab83d =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_8d90aaa10fcab83d_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @api_run@

    __defined at:__ @lib\/api.h 8:12@

    __exported by:__ @forward_typedef\/root.h@
-}
api_run ::
     BG.Ptr M.Foo.Foo
     -- ^ __C declaration:__ @f@
  -> BG.Ptr M.Bar.Bar
     -- ^ __C declaration:__ @b@
  -> IO M.Types.Lib_status
api_run = hs_bindgen_8d90aaa10fcab83d
