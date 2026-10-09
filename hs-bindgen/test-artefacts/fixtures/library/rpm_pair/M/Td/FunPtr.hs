{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Td.FunPtr
    ( M.Td.FunPtr.rpmtd_count
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Td
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <rpm_pair/td.h>"
  , "/* test_libraryrpm_pair_M.Td_get_rpmtd_count */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ce76ce72d8b80735 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtd_count;"
  , "}"
  ]))

-- __unique:__ @test_libraryrpm_pair_M.Td_get_rpmtd_count@
foreign import ccall unsafe "hs_bindgen_ce76ce72d8b80735" hs_bindgen_ce76ce72d8b80735_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryrpm_pair_M.Td_get_rpmtd_count@
hs_bindgen_ce76ce72d8b80735 :: IO (BG.FunPtr (Rpmtd -> IO BG.CInt))
hs_bindgen_ce76ce72d8b80735 =
  fmap BG.fromFFIType hs_bindgen_ce76ce72d8b80735_base

{-# NOINLINE rpmtd_count #-}
{-| __C declaration:__ @rpmtd_count@

    __defined at:__ @rpm_pair\/td.h 13:5@

    __exported by:__ @rpm_pair\/td.h@
-}
rpmtd_count :: BG.FunPtr (Rpmtd -> IO BG.CInt)
rpmtd_count =
  BG.unsafePerformIO hs_bindgen_ce76ce72d8b80735
