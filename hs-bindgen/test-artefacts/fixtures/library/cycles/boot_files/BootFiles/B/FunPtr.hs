{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module BootFiles.B.FunPtr
    ( BootFiles.B.FunPtr.b_by_value
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import BootFiles.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <boot_files.h>"
  , "/* test_librarycyclesboot_files_BootFiles.B_get_b_by_value */"
  , "__attribute__ ((const))"
  , "peer_enum (*hs_bindgen_f2b75505adba0666 (void)) ("
  , "  peer_enum arg1"
  , ")"
  , "{"
  , "  return &b_by_value;"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_get_b_by_value@
foreign import ccall unsafe "hs_bindgen_f2b75505adba0666" hs_bindgen_f2b75505adba0666_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_get_b_by_value@
hs_bindgen_f2b75505adba0666 :: IO (BG.FunPtr (Peer_enum -> IO Peer_enum))
hs_bindgen_f2b75505adba0666 =
  fmap BG.fromFFIType hs_bindgen_f2b75505adba0666_base

{-# NOINLINE b_by_value #-}
{-| __C declaration:__ @b_by_value@

    __defined at:__ @boot_files\/b.h 20:11@

    __exported by:__ @boot_files.h@
-}
b_by_value :: BG.FunPtr (Peer_enum -> IO Peer_enum)
b_by_value =
  BG.unsafePerformIO hs_bindgen_f2b75505adba0666
