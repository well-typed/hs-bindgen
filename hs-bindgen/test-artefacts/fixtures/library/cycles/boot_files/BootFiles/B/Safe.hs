{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module BootFiles.B.Safe
    ( BootFiles.B.Safe.b_by_value
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import BootFiles.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <boot_files.h>"
  , "peer_enum hs_bindgen_1d12d464f0d88aa9 ("
  , "  peer_enum arg1"
  , ")"
  , "{"
  , "  return (b_by_value)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_Safe_b_by_value@
foreign import ccall safe "hs_bindgen_1d12d464f0d88aa9" hs_bindgen_1d12d464f0d88aa9_base ::
     BG.CUInt
  -> IO BG.CUInt

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_Safe_b_by_value@
hs_bindgen_1d12d464f0d88aa9 ::
     Peer_enum
  -> IO Peer_enum
hs_bindgen_1d12d464f0d88aa9 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1d12d464f0d88aa9_base (BG.toFFIType x0))

{-| __C declaration:__ @b_by_value@

    __defined at:__ @boot_files\/b.h 20:11@

    __exported by:__ @boot_files.h@
-}
b_by_value ::
     Peer_enum
     -- ^ __C declaration:__ @e@
  -> IO Peer_enum
b_by_value = hs_bindgen_1d12d464f0d88aa9
