{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Td.Unsafe
    ( M.Td.Unsafe.rpmtd_count
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Td
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <rpm_pair/td.h>"
  , "signed int hs_bindgen_854c58206f90d04a ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return (rpmtd_count)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryrpm_pair_M.Td_Unsafe_rpmtd_count@
foreign import ccall unsafe "hs_bindgen_854c58206f90d04a" hs_bindgen_854c58206f90d04a_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_libraryrpm_pair_M.Td_Unsafe_rpmtd_count@
hs_bindgen_854c58206f90d04a ::
     Rpmtd
  -> IO BG.CInt
hs_bindgen_854c58206f90d04a =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_854c58206f90d04a_base (BG.toFFIType x0))

{-| __C declaration:__ @rpmtd_count@

    __defined at:__ @rpm_pair\/td.h 13:5@

    __exported by:__ @rpm_pair\/td.h@
-}
rpmtd_count ::
     Rpmtd
     -- ^ __C declaration:__ @td@
  -> IO BG.CInt
rpmtd_count = hs_bindgen_854c58206f90d04a
