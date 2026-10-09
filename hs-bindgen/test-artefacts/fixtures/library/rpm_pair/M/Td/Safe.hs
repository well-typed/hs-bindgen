{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Td.Safe
    ( M.Td.Safe.rpmtd_count
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Td
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <rpm_pair/td.h>"
  , "signed int hs_bindgen_9e754924eee4bd87 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return (rpmtd_count)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryrpm_pair_M.Td_Safe_rpmtd_count@
foreign import ccall safe "hs_bindgen_9e754924eee4bd87" hs_bindgen_9e754924eee4bd87_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_libraryrpm_pair_M.Td_Safe_rpmtd_count@
hs_bindgen_9e754924eee4bd87 ::
     Rpmtd
  -> IO BG.CInt
hs_bindgen_9e754924eee4bd87 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_9e754924eee4bd87_base (BG.toFFIType x0))

{-| __C declaration:__ @rpmtd_count@

    __defined at:__ @rpm_pair\/td.h 13:5@

    __exported by:__ @rpm_pair\/td.h@
-}
rpmtd_count ::
     Rpmtd
     -- ^ __C declaration:__ @td@
  -> IO BG.CInt
rpmtd_count = hs_bindgen_9e754924eee4bd87
