{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module BootFiles.A.FunPtr
    ( BootFiles.A.FunPtr.a_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import BootFiles.A
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <boot_files.h>"
  , "/* test_librarycyclesboot_files_BootFiles.A_get_a_new */"
  , "__attribute__ ((const))"
  , "struct a *(*hs_bindgen_3e550c71085e10ec (void)) ("
  , "  enum a_enum arg1"
  , ")"
  , "{"
  , "  return &a_new;"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesboot_files_BootFiles.A_get_a_new@
foreign import ccall unsafe "hs_bindgen_3e550c71085e10ec" hs_bindgen_3e550c71085e10ec_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycyclesboot_files_BootFiles.A_get_a_new@
hs_bindgen_3e550c71085e10ec :: IO (BG.FunPtr (A_enum -> IO (BG.Ptr A)))
hs_bindgen_3e550c71085e10ec =
  fmap BG.fromFFIType hs_bindgen_3e550c71085e10ec_base

{-# NOINLINE a_new #-}
{-| __C declaration:__ @a_new@

    __defined at:__ @a.h 19:11@

    __exported by:__ @boot_files.h@
-}
a_new :: BG.FunPtr (A_enum -> IO (BG.Ptr A))
a_new =
  BG.unsafePerformIO hs_bindgen_3e550c71085e10ec
