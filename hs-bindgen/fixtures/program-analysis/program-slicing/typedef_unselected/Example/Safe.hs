{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Int
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program-slicing/typedef_unselected.h>"
  , "void hs_bindgen_ef8f97cf27661c20 ("
  , "  U arg1"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_ef8f97cf27661c20" hs_bindgen_ef8f97cf27661c20_base ::
     GHC.Int.Int32
  -> IO ()

-- __unique:__ @test_programanalysisprogramslici_Example_Safe_bar@
hs_bindgen_ef8f97cf27661c20 ::
     U
  -> IO ()
hs_bindgen_ef8f97cf27661c20 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ef8f97cf27661c20_base

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/typedef_unselected.h 12:6@

    __exported by:__ @program-analysis\/program-slicing\/typedef_unselected.h@
-}
bar ::
     U
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_ef8f97cf27661c20
