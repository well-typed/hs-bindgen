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
  [ "#include <forward_typedef/root.h>"
  , "/* test_libraryforward_typedef_M.Api_get_api_run */"
  , "__attribute__ ((const))"
  , "lib_status (*hs_bindgen_498123b3babccdf1 (void)) ("
  , "  foo *arg1,"
  , "  bar *arg2"
  , ")"
  , "{"
  , "  return &api_run;"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_typedef_M.Api_get_api_run@
foreign import ccall unsafe "hs_bindgen_498123b3babccdf1" hs_bindgen_498123b3babccdf1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryforward_typedef_M.Api_get_api_run@
hs_bindgen_498123b3babccdf1 :: IO (BG.FunPtr (BG.Ptr M.Foo.Foo -> BG.Ptr M.Bar.Bar -> IO M.Types.Lib_status))
hs_bindgen_498123b3babccdf1 =
  fmap BG.fromFFIType hs_bindgen_498123b3babccdf1_base

{-# NOINLINE api_run #-}
{-| __C declaration:__ @api_run@

    __defined at:__ @lib\/api.h 8:12@

    __exported by:__ @forward_typedef\/root.h@
-}
api_run :: BG.FunPtr (BG.Ptr M.Foo.Foo -> BG.Ptr M.Bar.Bar -> IO M.Types.Lib_status)
api_run =
  BG.unsafePerformIO hs_bindgen_498123b3babccdf1
