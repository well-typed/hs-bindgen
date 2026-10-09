{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Api.Safe
    ( M.Api.Safe.api_run
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
  , "lib_status hs_bindgen_cf9e33824c44e2f3 ("
  , "  foo *arg1,"
  , "  bar *arg2"
  , ")"
  , "{"
  , "  return (api_run)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_typedef_M.Api_Safe_api_run@
foreign import ccall safe "hs_bindgen_cf9e33824c44e2f3" hs_bindgen_cf9e33824c44e2f3_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO HsBindgen.Runtime.Support.CInt

-- __unique:__ @test_libraryforward_typedef_M.Api_Safe_api_run@
hs_bindgen_cf9e33824c44e2f3 ::
     BG.Ptr M.Foo.Foo
  -> BG.Ptr M.Bar.Bar
  -> IO M.Types.Lib_status
hs_bindgen_cf9e33824c44e2f3 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_cf9e33824c44e2f3_base (BG.toFFIType x0) (BG.toFFIType x1))

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
api_run = hs_bindgen_cf9e33824c44e2f3
