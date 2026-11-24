{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module BootFiles.A.Safe
    ( BootFiles.A.Safe.a_new
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import BootFiles.A
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <boot_files.h>"
  , "struct a *hs_bindgen_2fa1bb522daacd86 ("
  , "  enum a_enum arg1"
  , ")"
  , "{"
  , "  return (a_new)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesboot_files_BootFiles.A_Safe_a_new@
foreign import ccall safe "hs_bindgen_2fa1bb522daacd86" hs_bindgen_2fa1bb522daacd86_base ::
     BG.CUInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarycyclesboot_files_BootFiles.A_Safe_a_new@
hs_bindgen_2fa1bb522daacd86 ::
     A_enum
  -> IO (BG.Ptr A)
hs_bindgen_2fa1bb522daacd86 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2fa1bb522daacd86_base (BG.toFFIType x0))

{-| __C declaration:__ @a_new@

    __defined at:__ @a.h 19:11@

    __exported by:__ @boot_files.h@
-}
a_new ::
     A_enum
     -- ^ __C declaration:__ @e@
  -> IO (BG.Ptr A)
a_new = hs_bindgen_2fa1bb522daacd86
