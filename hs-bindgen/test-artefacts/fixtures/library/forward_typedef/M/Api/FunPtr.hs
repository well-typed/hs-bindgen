{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Api.FunPtr
    ( M.Api.FunPtr.api_run
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Bar
import qualified M.Foo
import qualified M.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_forward_typedef_M.Api_get_api_run */"
  , "__attribute__ ((const))"
  , "lib_status (*hs_bindgen_9926e3c826e7e7f3 (void)) ("
  , "  foo *arg1,"
  , "  bar *arg2"
  , ")"
  , "{"
  , "  return &api_run;"
  , "}"
  ]))

-- __unique:__ @test_forward_typedef_M.Api_get_api_run@
foreign import ccall unsafe "hs_bindgen_9926e3c826e7e7f3" hs_bindgen_9926e3c826e7e7f3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_forward_typedef_M.Api_get_api_run@
hs_bindgen_9926e3c826e7e7f3 :: IO (BG.FunPtr (BG.Ptr M.Foo.Foo -> BG.Ptr M.Bar.Bar -> IO M.Types.Lib_status))
hs_bindgen_9926e3c826e7e7f3 =
  fmap BG.fromFFIType hs_bindgen_9926e3c826e7e7f3_base

{-# NOINLINE api_run #-}
{-| __C declaration:__ @api_run@

    __defined at:__ @lib\/api.h 6:12@

    __exported by:__ @root.h@
-}
api_run :: BG.FunPtr (BG.Ptr M.Foo.Foo -> BG.Ptr M.Bar.Bar -> IO M.Types.Lib_status)
api_run =
  BG.unsafePerformIO hs_bindgen_9926e3c826e7e7f3
