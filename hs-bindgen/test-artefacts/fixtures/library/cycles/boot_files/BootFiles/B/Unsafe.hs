{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module BootFiles.B.Unsafe
    ( BootFiles.B.Unsafe.b_by_value
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import BootFiles.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <boot_files.h>"
  , "peer_enum hs_bindgen_b37478a298c913b1 ("
  , "  peer_enum arg1"
  , ")"
  , "{"
  , "  return (b_by_value)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_Unsafe_b_by_value@
foreign import ccall unsafe "hs_bindgen_b37478a298c913b1" hs_bindgen_b37478a298c913b1_base ::
     BG.CUInt
  -> IO BG.CUInt

-- __unique:__ @test_librarycyclesboot_files_BootFiles.B_Unsafe_b_by_value@
hs_bindgen_b37478a298c913b1 ::
     Peer_enum
  -> IO Peer_enum
hs_bindgen_b37478a298c913b1 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b37478a298c913b1_base (BG.toFFIType x0))

{-| __C declaration:__ @b_by_value@

    __defined at:__ @boot_files\/b.h 20:11@

    __exported by:__ @boot_files.h@
-}
b_by_value ::
     Peer_enum
     -- ^ __C declaration:__ @e@
  -> IO Peer_enum
b_by_value = hs_bindgen_b37478a298c913b1
